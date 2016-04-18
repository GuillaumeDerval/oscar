/*******************************************************************************
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
 ******************************************************************************/

package oscar.cp.core

import java.util.Collection
import java.util.LinkedList
import scala.collection.JavaConversions.asJavaCollection
import scala.collection.JavaConversions.collectionAsScalaIterable
import oscar.algo.array.ArrayQueue
import oscar.algo.reversible.ReversiblePointer
import oscar.algo.search.SearchNode
import oscar.cp.constraints.EqCons
import oscar.cp.core.CPOutcome.Failure
import oscar.cp.core.CPOutcome.Success
import oscar.cp.core.CPOutcome.Suspend
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPSetVar
import oscar.cp.core.watcher.PropagEventQueueVarSet
import oscar.cp.core.watcher.PropagEventQueueVarInt
import oscar.algo.search.DFSearchNode
import scala.util.Random

/**
 * Constraint Programming CPStore
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class CPStore(final val propagStrength: CPPropagStrength, hotDegree: Int) extends DFSearchNode {
  
  def this(defaultPropag: CPPropagStrength) = this(defaultPropag, 2)

  def this() = this(CPPropagStrength.Weak)
  
  // Random object
  private[this] val rand = new Random(0)

  // Propagation queue L1 (AC5)
  private[this] val propagQueueL1 = Array.fill(CPStore.MaxPriorityL1 + 1)(new ArrayQueue[() => CPOutcome](1000))
  private[this] var highestPriorL1 = -1

  // Propagation queue L2 (AC3)
  private[this] val propagQueueL2 = Array.fill(CPStore.MaxPriorityL2 * hotDegree + 1)(new ArrayQueue[Constraint](100))
  private[this] var highestPriorL2 = -1

  private[this] val cutConstraints = new ArrayQueue[Constraint](1) // usually empty

  // Total time spent in the fixed point algorithm
  private[this] var timeInFixedPoint: Long = 0

  // True if the store is executing the fixed point algorithm
  private[this] var inFixedPoint = false
  
  // Number of times an L1 filtering is called during the fix point
  private[this] var nCallsL1 = 0L  
  
  // Number of times an L1 filtering is called during the fix point
  private[this] var nCallsL2 = 0L   
  
  def resetStatistics() {
    timeInFixedPoint = 0
    nCallsL1 = 0
    nCallsL2 = 0
  }
  
  def statistics = new SolverStatistics(nCallsL1,nCallsL2,timeInFixedPoint)
  

  // Reference to the last constraint called
  private[this] var lastConstraint: Constraint = null
  
  final def getRandom(): Random = rand

  /**
   *  Returns the last constraint called in the propagate algorithm.
   *
   *  Note that `null` is returned if no constraint has been called.
   *
   *  @return The last constraint called by the propagate algorithm.
   */
  def lastConstraintCalled: Constraint = lastConstraint

  // Cleans the propagation queues
  @inline protected def cleanQueues(): Unit = {
    // Clean queue L1
    highestPriorL1 = -1
    var i = propagQueueL1.length
    while (i > 0) {
      i -= 1
      propagQueueL1(i).clear()
    }
    // Clean queue L2
    highestPriorL2 = -1
    i = propagQueueL2.length
    while (i > 0) {
      i -= 1
      propagQueueL2(i).clear()
    }
  }

  // Adds the constraint in the L2 queue
  @inline final def enqueueL2(c: Constraint): Unit = {
    if (c.isEnqueuable) {
      c.setInQueue()
      val priority = c.priorityL2
      propagQueueL2(priority).addLast(c)
      if (priority > highestPriorL2) {
        highestPriorL2 = priority
      }
    }
  }
  
  @inline final def enqueueL2(c: Constraint, hot: Int): Unit = {
    if (c.isEnqueuable) {
      c.setInQueue()
      val priority = c.priorityL2 * hotDegree + hot
      propagQueueL2(priority).addLast(c)
      if (priority > highestPriorL2) {
        highestPriorL2 = priority
      }
    }
  }

  // Adds the constraint in the L1 queue
  @inline final def enqueueL1(c: Constraint, priority: Int, evt: => CPOutcome): Unit = {
    propagQueueL1(priority).addLast(() => {
      if (c.isActive) {
        lastConstraint = c // last constraint called
        val oc = evt
        if (oc == Success) c.deactivate()
        oc
      } else Suspend
    })
    if (priority > highestPriorL1) {
      highestPriorL1 = priority
    }
  }

  // set variable

  def notifyRequired(constraints: PropagEventQueueVarSet, x: CPSetVar, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        enqueueL1(c, c.priorityBindL1, c.valRequired(x, v))
      }
      q = q.next
    }
  }

  def notifyRequiredIdx(constraints: PropagEventQueueVarSet, x: CPSetVar, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        enqueueL1(c, c.priorityBindL1, c.valRequiredIdx(x, idx, v))
      }
      q = q.next
    }
  }

  def notifyExcluded(constraints: PropagEventQueueVarSet, x: CPSetVar, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        enqueueL1(c, c.priorityBindL1, c.valExcluded(x, v))
      }
      q = q.next
    }
  }

  def notifyExcludedIdx(constraints: PropagEventQueueVarSet, x: CPSetVar, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        enqueueL1(c, c.priorityBindL1, c.valExcludedIdx(x, idx, v))
      }
      q = q.next
    }
  }

  final def doAndPropagate(action: => CPOutcome): CPOutcome = {
    val out = action // apply action
    if (out == Failure) Failure
    else if (isFailed()) Failure
    else propagate()
  }
  
  /**
   *  Call the propagate function of the constraints and trigger the fix point algorithm
   *
   *  Note that the constraints are not added to the model
   *
   *  @param constraints a sequence of constraints
   */
  def propagate(constraints: Constraint*): CPOutcome = {
    if (isFailed()) Failure
    else {
      constraints.foreach(c => enqueueL2(c))
      propagate()
      if (isFailed()) Failure
      else Suspend
    }
  }

  protected def propagate(): CPOutcome = {
    if (isFailed()) throw Inconsistency
    else {
      val t = System.currentTimeMillis()
      inFixedPoint = true
      try {
        val outcome = fixedPoint()
        if (outcome == Failure) {
          cleanQueues() // may be not empty
          fail()
        }
        outcome
      } catch {
        case i: Inconsistency => {
          cleanQueues() // may be not empty
          fail()
          Failure
        }
      } finally {
        timeInFixedPoint += System.currentTimeMillis() - t
        inFixedPoint = false
      }
    }
  }

  @inline private def fixedPoint(): CPOutcome = {

    // Adds the cut constraints
    cutConstraints.foreach(c => enqueueL2(c))

    var isNotFailed = true
    while (isNotFailed && (highestPriorL1 >= 0 || highestPriorL2 >= 0)) {

      // Propagate L1
      while (highestPriorL1 >= 0 && isNotFailed) {
        val queue = propagQueueL1(highestPriorL1)
        if (queue.isEmpty) highestPriorL1 -= 1
        else {
          nCallsL1 += 1
          val event = queue.removeFirst()
          isNotFailed = event() != Failure
        }
      }

      // Propagate L2 if no constraint in L1
      while (highestPriorL1 < 0 && highestPriorL2 >= 0 && isNotFailed) {
        val queue = propagQueueL2(highestPriorL2)
        if (queue.isEmpty) highestPriorL2 -= 1
        else {
          nCallsL2 += 1
          val constraint = queue.removeFirst()
          lastConstraint = constraint
          isNotFailed = constraint.execute() != Failure
        }
      }
    }

    if (isNotFailed) Suspend
    else Failure
  }

  def printQueues(): Unit = {
    println("----------")
    propagQueueL1.foreach(q => println("L1: " + q.size))
    propagQueueL2.foreach(q => println("L2: " + q.size))
  }

  /**
   * Add a constraint to the store in a reversible way and trigger the fix-point algorithm. <br>
   * In a reversible way means that the constraint is present in the store only for descendant nodes.
   * @param c
   * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint).
   * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
   */
  def post(constraint: Constraint, st: CPPropagStrength): CPOutcome = {
    if (isFailed()) Failure
    else if (inFixedPoint) { // already in a try block
      val outcome = constraint.setup(st)
      if (outcome == Success) {
         constraint.deactivate()
         Success
      }
      else if (outcome == Failure) {
        fail()
        Failure
      }
      else Suspend
    } else try {
      val outcome = constraint.setup(st)
      if (outcome == Failure) {
        fail()
        Failure
      } else {
        if (outcome == Success) constraint.deactivate()
        propagate()
      }
    } catch {
      case i: Inconsistency => {
        fail()
        Failure
      }
    }
  }

  def post(c: Constraint): CPOutcome = post(c, propagStrength)

  def postCut(c: Constraint): CPOutcome = postCut(c, propagStrength)

  def postCut(c: Constraint, st: CPPropagStrength): CPOutcome = {
    val ok = post(c, st);
    cutConstraints.addLast(c);
    return ok;
  }

  def resetCuts(): Unit = {
    for (c <- cutConstraints) {
      c.deactivate() // we cannot really remove them because they were set-up
    }
    cutConstraints.clear()
  }

  /**
   * Add a constraint b == true to the store (with a Weak propagation strength) in a reversible way and trigger the fix-point algorithm. <br>
   * In a reversible way means that the constraint is present in the store only for descendant nodes.
   * @param c, the constraint
   * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise
   */
  def post(b: CPBoolVar): CPOutcome = post(new EqCons(b, 1), propagStrength)

  /**
   * Add a set of constraints to the store in a reversible way and trigger the fix-point algorithm afterwards.
   * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
   * @param constraints
   * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint)
   * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
   */
  def post(constraints: Array[Constraint], st: CPPropagStrength): CPOutcome = {
    if (isFailed()) Failure
    else if (inFixedPoint) { // already in a try block
      var outcome: CPOutcome = Suspend
      var i = 0
      while (i < constraints.length && outcome != Failure) {
        val constraint = constraints(i)
        outcome = constraint.setup(st)
        if (outcome == Success) constraint.deactivate()
        i += 1
      }
      if (outcome == Failure) fail()
      outcome
    } else try {
      var outcome: CPOutcome = Suspend
      var i = 0
      while (i < constraints.length && outcome != Failure) {
        val constraint = constraints(i)
        outcome = constraint.setup(st)
        if (outcome == Success) constraint.deactivate()
        i += 1
      }
      if (outcome != Failure) propagate()
      else {
        fail()
        Failure
      }
    } catch {
      case i: Inconsistency => {
        fail()
        Failure
      }
    }
  }

  def post(constraints: Array[Constraint]): CPOutcome = post(constraints, propagStrength);

  /**
   * Add a set of constraints to the store (with a Weak propagation strength) in a reversible way and trigger the fix-point algorithm afterwards.
   * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
   * @param constraints
   * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
   */
  def post(constraints: Collection[Constraint], st: CPPropagStrength): CPOutcome = post(constraints.map(x => x.asInstanceOf[Constraint]).toArray, st)

  def post(constraints: Collection[Constraint]): CPOutcome = post(constraints.map(x => x.asInstanceOf[Constraint]), propagStrength)

  def assign(x: CPIntVar, v: Int): CPOutcome = {
    if (isFailed()) Failure
    else try {
      val outcome = x.assign(v)
      if (outcome != Failure) propagate()
      else {
        fail()
        Failure
      }
    } catch {
      case i: Inconsistency => {
        fail()
        Failure
      }
    }
  }

  def remove(x: CPIntVar, v: Int): CPOutcome = {
    if (isFailed()) Failure
    else try {
      val outcome = x.removeValue(v)
      if (outcome != Failure) propagate()
      else {
        fail()
        Failure
      }
    } catch {
      case i: Inconsistency => {
        fail()
        Failure
      }
    }
  }
  
  def smallerEq(x: CPIntVar, v: Int): CPOutcome = {
    if (isFailed()) Failure
    else try {
      val outcome = x.updateMax(v)
      if (outcome != Failure) propagate()
      else {
        fail()
        Failure
      }
    } catch {
      case i: Inconsistency => {
        fail()
        Failure
      }
    }
  }
  
  def largerEq(x: CPIntVar, v: Int): CPOutcome = {
    if (isFailed()) Failure
    else try {
      val outcome = x.updateMin(v)
      if (outcome != Failure) propagate()
      else {
        fail()
        Failure
      }
    } catch {
      case i: Inconsistency => {
        fail()
        Failure
      }
    }
  }  
  

  def add(c: Constraint, st: CPPropagStrength): CPOutcome = post(c, st)

  def add(c: Constraint): CPOutcome = add(c, propagStrength)

  def add(b: CPBoolVar): CPOutcome = post(new EqCons(b, 1))
  
  def addCut(c: Constraint): CPOutcome = postCut(c)

  /**
   * Add a set of constraints to the store in a reversible way and trigger the fix-point algorithm afterwards.
   * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
   * @param constraints
   * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint)
   * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
   */
  def add(constraints: Array[Constraint], st: CPPropagStrength): CPOutcome = post(constraints, st)

  def add(constraints: Array[Constraint]): CPOutcome = add(constraints, propagStrength)

  def add(constraints: Iterable[Constraint], st: CPPropagStrength): CPOutcome = add(constraints.toArray, st)

  def add(constraints: Iterable[Constraint]): CPOutcome = add(constraints.toArray, propagStrength)

  def +=(c: Constraint, st: CPPropagStrength): CPOutcome = add(c, st)
  def +=(c: Constraint): CPOutcome = add(c, propagStrength)
}

object CPStore {

  /** The highest priority for an Level 1 filtering method */
  @deprecated val MAXPRIORL1 = 2

  /** The highest priority for the propagate method i.e. L2 */
  @deprecated val MAXPRIORL2 = 7

  /** The highest priority for an Level 1 filtering method */
  val MaxPriorityL1 = 2

  /** The lowest priority for an Level 1 filtering method */
  val MinPriorityL1 = 0

  /** The highest priority for an Level 2 filtering method */
  val MaxPriorityL2 = 7

  /** The lowest priority for an Level 2 filtering method */
  val MinPriorityL2 = 0
}

class SolverStatistics(
  val nCallsL1: Long,
  val nCallsL2: Long,
  val timeInFixPoint: Long) {
  override val toString: String = s"nCallsL1: $nCallsL1\nnCallsL2: $nCallsL2\ntimeInFixedPoint(ms): $timeInFixPoint"
}
