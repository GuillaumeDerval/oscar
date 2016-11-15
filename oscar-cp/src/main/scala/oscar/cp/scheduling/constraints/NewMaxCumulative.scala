/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */
package oscar.cp.scheduling.constraints


/*
import oscar.algo.EfficientHeap
import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.scheduling.MirrorCumulativeActivity
import oscar.cp.modeling.CPSolver
import oscar.cp.core.CPIntVar
import oscar.cp.core.Outcome
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength

class NewMaxCumulative(cp: CPSolver, allTasks: Array[CumulativeActivity], limit: Int, r: Int) extends Constraint(cp, "NewMaxCumulative") {

  // The tasks (one array by direction of the sweep)
  val lToRTasks: Array[CumulativeActivity] = allTasks.filter(_.resource.hasValue(r))
  val rToLTasks: Array[CumulativeActivity] = lToRTasks.map(new MirrorCumulativeActivity(_))

  val nTasks = lToRTasks.size
  val Tasks = 0 until nTasks

  // The events to process (min heap on date)
  val hEvents = new EfficientEventHeap(4 * nTasks)

  // The tasks to check (max heap on resource)
  val hCheck = new EfficientHeap[(Int, Int)](nTasks, (a, b) => a._1 > b._1)
  // This array is used to know if a task is in hCheck
  val hCheckContent = new Array[Boolean](nTasks)

  // The tasks in conflict with the sweep line (min heap on resource)
  val hConflict = new EfficientHeap[(Int, Int)](nTasks, (a, b) => a._1 < b._1)
  // This array is used to know if a task is in hConflict
  val hConflictContent = new Array[Boolean](nTasks)

  val evup = new Array[Boolean](nTasks)
  val mins = new Array[Int](nTasks)

  // Sweep line parameters
  val newActiveTasks = new Array[Int](nTasks)
  var nActiveTasks = 0
  var delta = 0
  var deltaBis = 0
  var gap = 0

  // Events are preprocessed to speed-up the propagation
  val eventList = Array.tabulate(nTasks) { e => new EventList(e) }

  override def setup(l: CPPropagStrength): Outcome = {

    // Treat this constraint at the end of the propagation queue
    priorityL2 = 0
    // Propagate is sufficient to reach the fix point of this constraint
    idempotent = true

    val oc = propagate()

    if (oc == Outcome.Suspend) {
      for (i <- Tasks) {

        if (!lToRTasks(i).start.isBound) lToRTasks(i).start.callPropagateWhenBoundsChange(this)
        if (!lToRTasks(i).dur.isBound) lToRTasks(i).dur.callPropagateWhenBoundsChange(this)
        if (!lToRTasks(i).end.isBound) lToRTasks(i).end.callPropagateWhenBoundsChange(this)
        if (!lToRTasks(i).height.isBound) lToRTasks(i).height.callPropagateWhenBoundsChange(this)
        if (!lToRTasks(i).resource.isBound) lToRTasks(i).resource.callPropagateWhenDomainChanges(this)
      }
    }

    return oc
  }

  override def propagate(): Outcome = {

    // Left to Right sweep
    if (sweepAlgorithm(lToRTasks) == Outcome.Failure) return Outcome.Failure

    // Right to Left sweep
    if (sweepAlgorithm(rToLTasks) == Outcome.Failure) return Outcome.Failure

    return Outcome.Suspend
  }

  def initDataStructures(tasks: Array[CumulativeActivity]) {

    hEvents.clear
    generateEvents(tasks)

    hCheck.clear
    for (i <- Tasks) hCheckContent(i) = false

    hConflict.clear
    for (i <- Tasks) hConflictContent(i) = false

    //newActiveTasks.clear
    nActiveTasks = 0

    delta = hEvents.head.date
    deltaBis = hEvents.head.date
    gap = limit
  }

  def generateEvents(tasks: Array[CumulativeActivity]) {

    for (i <- Tasks; if (tasks(i).resource.isBoundTo(r))) {

      if (tasks(i).lst < tasks(i).ect) {

        // Those events represent the compulsory part
        hEvents enqueue eventList(i).getSCP(tasks)
        hEvents enqueue eventList(i).getECPD(tasks)
      }

      if (tasks(i).lst >= tasks(i).ect) {

        // Reevaluation of the compulsory part
        hEvents enqueue eventList(i).getCCP(tasks)
      }

      if (tasks(i).est != tasks(i).lst) {

        // The task is considered in the pruning process
        hEvents enqueue eventList(i).getPR(tasks)
      }
    }
  }

  def sweepAlgorithm(tasks: Array[CumulativeActivity]): Outcome = {

    // INITIALIZATION
    initDataStructures(tasks)

    while (!hEvents.isEmpty) {

      // HANDLING THE SWEEP-LINE MOVE
      if (delta != deltaBis) {

        var i = 0
        while (i < nActiveTasks) {

          val t = newActiveTasks(i)

          if (tasks(t).minHeight > gap) {

            hConflict.enqueue((tasks(t).minHeight, t))
            hConflictContent(t) = true

          } else if (tasks(t).minDuration > deltaBis - delta) {

            hCheck.enqueue((tasks(t).minHeight, t))
            hCheckContent(t) = true
            mins(t) = delta

          } else {
            evup(t) = true
          }

          i += 1
        }
        // All the active tasks have be processed
        nActiveTasks = 0

        if (!filterMin(tasks, delta, deltaBis)) return Outcome.Failure

        delta = deltaBis
      }

      // HANDLING CURRENT EVENT
      delta = synchronize(tasks, delta)

      val event = hEvents.dequeue

      if (event.isSCP || event.isECPD) {

        gap = gap + event.dec

      } else if (event.isPR) {

        newActiveTasks(nActiveTasks) = event.task
        nActiveTasks += 1
      }

      // GETTING NEXT EVENT
      if (hEvents.isEmpty && !filterMin(tasks, delta, Int.MaxValue)) return Outcome.Failure

      deltaBis = synchronize(tasks, delta)
    }

    return Outcome.Suspend
  }

  def filterMin(tasks: Array[CumulativeActivity], delta: Int, deltaBis: Int): Boolean = {

    if (gap < 0)
      return false

    // UPDATING TOP TASKS OF hCheck

    while (!hCheck.isEmpty && (hEvents.isEmpty || hCheck.head._1 > gap)) {

      val check = hCheck.dequeue
      val t = check._2
      val h = check._1
      hCheckContent(t) = false

      if (delta >= tasks(t).lst || delta - mins(t) >= tasks(t).minDuration || hEvents.isEmpty) {

        if (tasks(t).adjustStart(mins(t)) == Outcome.Failure) return false

        if (!evup(t)) {
          //Update events of the compulsory part of t
          evup(t) = true
        }
      } else {
        hConflict.enqueue(check)
        hConflictContent(t) = true
      }
    }

    // UPDATING TOP TASKS OF hConflict

    while (!hConflict.isEmpty && hConflict.head._1 <= gap) {

      val conflict = hConflict.dequeue
      val t = conflict._2
      val h = conflict._1
      hConflictContent(t) = false

      if (delta >= tasks(t).lst) {

        if (tasks(t).adjustStart(tasks(t).lst) == Outcome.Failure) return false

        if (!evup(t)) {
          //Update events of the compulsory part of t
          evup(t) = true
        }
      } else {

        if (deltaBis - delta >= tasks(t).minDuration) {

          if (tasks(t).adjustStart(delta) == Outcome.Failure) return false

          tasks(t).adjustStart(delta)

          if (!evup(t)) {

            //Update events of the compulsory part of t
            evup(t) = true
          }
        } else {

          hCheck.enqueue(conflict)
          hCheckContent(t) = true
          mins(t) = delta
        }
      }
    }

    true
  }

  def synchronize(tasks: Array[CumulativeActivity], delta: Int): Int = {

    var sync = false
    var event: Event = null

    // UPDATING TOP EVENTS
    do {

      if (hEvents.isEmpty) return -1

      sync = true

      event = hEvents.head

      // PROCESSING DYNAMIC EVENT
      if (event.isECPD && !evup(event.task)) {

        if (hCheckContent(event.task)) {

          hEvents.dequeue
          hEvents.enqueue(new Event(event.eType, event.task, mins(event.task) + tasks(event.task).minDuration, event.dec))

        } else if (hConflictContent(event.task)) {

          hEvents.dequeue
          hEvents.enqueue(new Event(event.eType, event.task, tasks(event.task).lst + tasks(event.task).minDuration, event.dec))
        }

        evup(event.task) = true
        sync = false

        // PROCESSING CONDITIONAL EVENT	
      } else if (event.isCCP && !evup(event.task) && event.date == delta) {

        if (hCheckContent(event.task) && mins(event.task) + tasks(event.task).minDuration > delta) {

          hEvents.enqueue(new Event(EventType.SCP, event.task, delta, -tasks(event.task).minHeight))
          hEvents.enqueue(new Event(EventType.ECPD, event.task, mins(event.task) + tasks(event.task).minDuration, tasks(event.task).minHeight))

        } else if (hConflictContent(event.task)) {

          hEvents.enqueue(new Event(EventType.SCP, event.task, delta, -tasks(event.task).minHeight))
          hEvents.enqueue(new Event(EventType.ECPD, event.task, tasks(event.task).lct, tasks(event.task).minHeight))
        }

        evup(event.task) = true
        sync = false
      }

    } while (!sync)

    return event.date
  }

  /**
   * The Event
   */
  protected object EventType {

    val SCP = 0
    val ECPD = 1
    val CCP = 2
    val PR = 3

    def eventToString(i: Int) = {
      i match {
        case 0 => "SCP"
        case 1 => "ECPD"
        case 2 => "CCP"
        case 3 => "PR"
        case _ => "unknown event"
      }
    }
  }

  class Event(e: Int, t: Int, private var d: Int, private var inc: Int) extends Enumeration {

    def isSCP = { e == EventType.SCP }
    def isECPD = { e == EventType.ECPD }
    def isCCP = { e == EventType.CCP }
    def isPR = { e == EventType.PR }

    def date = d
    def eType = e
    def dec = inc
    def task = t

    def date_=(x: Int) { d = x }
    def dec_=(x: Int) { inc = x }

    override def toString = { "<" + e + ", " + t + ", " + d + ", " + inc + ">" }
  }

  class EventList(t: Int) {

    val SCP: Event = new Event(EventType.SCP, t, 0, 0)
    val ECPD: Event = new Event(EventType.ECPD, t, 0, 0)
    val CCP: Event = new Event(EventType.CCP, t, 0, 0)
    val PR: Event = new Event(EventType.PR, t, 0, 0)

    def getSCP(tasks: Array[CumulativeActivity]): Event = {
      SCP.date = tasks(t).lst
      SCP.dec = -tasks(t).minHeight
      return SCP
    }

    def getECPD(tasks: Array[CumulativeActivity]): Event = {
      ECPD.date = tasks(t).ect
      ECPD.dec = tasks(t).minHeight
      return ECPD
    }

    def getCCP(tasks: Array[CumulativeActivity]): Event = {
      CCP.date = tasks(t).lst
      return CCP
    }

    def getPR(tasks: Array[CumulativeActivity]): Event = {
      PR.date = tasks(t).est
      return PR
    }
  }

  class EfficientEventHeap(maxSize: Int) {

    private val heap: Array[Event] = new Array(maxSize + 1)

    private var heapSize = 0

    def head = heap(1)

    def size = heapSize

    def isEmpty = (size == 0)

    def clear() { heapSize = 0 }

    def enqueue(event: Event) {

      heapSize += 1

      var i = heapSize
      heap(i) = event

      while (i > 1 && heap(parent(i)).date > heap(i).date) {

        val temp = heap(i)
        val p = parent(i)

        heap(i) = heap(p)
        heap(p) = temp
        i = p
      }
    }

    def dequeue(): Event = {

      if (heapSize < 1)
        throw new NoSuchElementException

      val max: Event = heap(1)
      heap(1) = heap(heapSize)
      heapSize -= 1

      heapify(1)

      return max
    }

    def heapify(j: Int) {

      var largest = j
      var i = 0

      do {
        i = largest

        val l = left(i)
        val r = right(i)

        if (l <= heapSize && heap(l).date < heap(i).date)
          largest = l
        else
          largest = i

        if (r <= heapSize && heap(r).date < heap(largest).date)
          largest = r

        if (largest != i) {
          val temp = heap(i)
          heap(i) = heap(largest)
          heap(largest) = temp
        }

      } while (largest != i)
    }

    def parent(i: Int) = i / 2
    def left(i: Int) = 2 * i
    def right(i: Int) = 2 * i + 1
  }

}*/
