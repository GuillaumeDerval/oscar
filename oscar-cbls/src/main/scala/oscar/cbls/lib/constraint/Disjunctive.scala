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


package oscar.cbls.lib.constraint

import oscar.cbls.core.computation.CBLSIntVar._
import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.{SortedSet, SortedMap}


/**
 * Implement the Disjunctive constraint.
 * @param start: the start time of each task
 * @param duration: the duration of each task
 * @author Jean-Noël Monette 
 */
case class DisjunctiveConstDuration(start: Array[IntValue],
                                    duration: Array[Int]) extends Invariant with Constraint with IntNotificationTarget{

  registerStaticAndDynamicDependencyArrayIndex(start)
  registerConstrainedVariables(start)
  finishInitialization()

  private val sumdur = duration.foldLeft(0)((acc,v) => acc + v);

  private val Violation: CBLSIntVar = new CBLSIntVar(model, 0, (0 to sumdur*start.length), "ViolationOfDisjunctive")
  Violation.setDefiningInvariant(this)


  //the degree of violation of a task is the sum of the sizes of its overlap with other tasks.
  private val Violations: SortedMap[IntValue, CBLSIntVar] = start.foldLeft(
    SortedMap.empty[IntValue, CBLSIntVar])(
    (acc, intvar) => {
      val newvar = new CBLSIntVar(model, 0, (0 to sumdur), "Violation_Disjunctive_" + intvar.name)
      acc + ((intvar, newvar))
    })

  for(i <- 0 until start.length){
    val curstart = start(i).value
    val curduration = duration(i)
    val curend = curstart+curduration
    for(j <- i+1 until start.length){
      val nextstart = start(j).value
      val nextduration = duration(j)
      val nextend = nextstart + nextduration
      val overlap = math.max(0,math.min(math.min(nextend-curstart,curend-nextstart),math.min(curduration,nextduration)))
      Violations(start(i)) :+= overlap
      Violations(start(j)) :+= overlap
      Violation :+= overlap
    }
  }

  //oldstarts is used to make sure that only one variable is handled at a time.
  private val oldstarts = start.map(v => v.value)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, oldstart: Int, newstart: Int) {
    //TODO: This is not completely incremental (but still linear instead of quadratic)!
    val dur = duration(index)
    val oldend = oldstart + dur
    val newend = newstart + dur
    for(j <- 0 until start.length){
      if(j!=index){
        val js = oldstarts(j)
        val jd = duration(j)
        val je = js + jd
        val oldoverlap = math.max(0,math.min(math.min(je-oldstart,oldend-js),math.min(dur,jd)))
        val newoverlap = math.max(0,math.min(math.min(je-newstart,newend-js),math.min(dur,jd)))
        Violations(v) :+= (newoverlap-oldoverlap)
        Violations(start(j)) :+= (newoverlap-oldoverlap)
        Violation :+= (newoverlap-oldoverlap)
      }
    }
    oldstarts(index) = newstart
  }

  override def violation = Violation

  override def violation(v: Value): IntValue = {
    Violations(v.asInstanceOf[IntValue])
  }

  override def checkInternals(c: Checker) {
    //TODO
  }
}


/**
 * Implement the Disjunctive constraint.
 * @param start: the start time of each task
 * @param duration: the duration of each task
 * @author Jean-Noël Monette
 * @author Renaud De Landtsheer
 */
case class Disjunctive(start: Array[IntValue],
                       duration: Array[IntValue]) extends Invariant with Constraint with IntNotificationTarget{

  require(start.length == duration.length,"start and duration array do not have the same size?!")

  val taskIndices:Range = start.indices
  val nbTask = start.length

  registerStaticAndDynamicDependencyArrayIndex(start)
  registerStaticAndDynamicDependencyArrayIndex(duration,-nbTask)

  registerConstrainedVariables(start)
  finishInitialization()

  private val sumMaxDur = duration.foldLeft(0)((acc,v) => acc + v.max);

  private val violationVar: CBLSIntVar = new CBLSIntVar(model, 0, (0 to sumMaxDur*start.length), "ViolationOfDisjunctive")
  violationVar.setDefiningInvariant(this)

  //the degree of violation of a task is the sum of the sizes of its overlap with other tasks.
  //opnly tasks with nonZero durations are taken into account (obviously)
  private val violationsVars: SortedMap[IntValue, CBLSIntVar] = start.foldLeft(
    SortedMap.empty[IntValue, CBLSIntVar])(
    (acc, intvar) => {
      val newvar = new CBLSIntVar(model, 0, (0 to sumMaxDur), "Violation_Disjunctive_" + intvar.name)
      acc + ((intvar, newvar))
    })

  private var nonZeroTasks:SortedSet[Int] = SortedSet.empty[Int] ++ taskIndices.filter(i => duration(i).value != 0)

  for(taskID <- nonZeroTasks){
    val curStart = start(taskID).value
    val curDuration = duration(taskID).value
    val curEnd = curStart + curDuration

    for(otherTaskID <- nonZeroTasks if taskID < otherTaskID){
      val otherStart = start(otherTaskID).value
      val otherDuration = duration(otherTaskID).value
      val otherEnd = otherStart + otherDuration
      val overlap = math.max(0,
        math.min(
          math.min(otherEnd-curStart,curEnd-otherStart),
          math.min(curDuration,otherDuration)))
      violationsVars(start(taskID)) :+= overlap
      violationsVars(start(otherTaskID)) :+= overlap
      violationVar :+= overlap
    }
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, oldValue: Int, newValue: Int) {
    if(index <0){
      //a duration has changed
      notifyDurChanged(index + nbTask,oldValue, newValue)
    }else{
      // a start has changed
      notifyStartChanged(index,oldValue, newValue)
    }
  }

  def notifyStartChanged(taskID:Int,oldStart:Int,newStart:Int) {
    val dur = duration(taskID).value
    if (dur == 0) return

    val oldEnd = oldStart + dur
    val newEnd = newStart + dur

    updateTask(taskID,oldStart,newStart,oldEnd,newEnd)
  }

  def notifyDurChanged(taskID:Int,oldDur:Int,newDur:Int){
    if(oldDur == 0 && newDur !=0){
      nonZeroTasks = nonZeroTasks + taskID
    }else if (oldDur !=0 && newDur == 0){
      nonZeroTasks = nonZeroTasks - taskID
    }

    val startTask = start(taskID).value
    val oldEnd = startTask + oldDur
    val newEnd = startTask + newDur

    updateTask(taskID,startTask,startTask,oldEnd,newEnd)
  }

  def updateTask(taskID:Int,oldStart:Int,newStart:Int,oldEnd:Int,newEnd:Int){
    //TODO: This is not completely incremental (but still linear instead of quadratic)!
    val oldDuration = oldEnd - oldStart
    val newDuration = newEnd - newStart
    for(otherTaskID <- nonZeroTasks if taskID != otherTaskID){
      val otherStart = start(otherTaskID).value
      val otherDuration = duration(otherTaskID).value
      val otherEnd = otherStart + otherDuration
      val oldOverlap = math.max(0,math.min(math.min(otherEnd-oldStart,oldEnd-otherStart),math.min(oldDuration,otherDuration)))
      val newOverlap = math.max(0,math.min(math.min(otherEnd-newStart,newEnd-otherStart),math.min(newDuration,otherDuration)))
      val deltaViolation = newOverlap - oldOverlap
      if(deltaViolation !=0) {
        violationsVars(duration(taskID)) :+= deltaViolation
        violationsVars(duration(taskID)) :+= deltaViolation
        violationsVars(start(otherTaskID)) :+= deltaViolation
        violationsVars(start(otherTaskID)) :+= deltaViolation
        violationVar :+= deltaViolation
      }
    }
  }

  override def violation = violationVar

  override def violation(v: Value): IntValue = {
    violationsVars(v.asInstanceOf[IntValue])
  }

  override def checkInternals(c: Checker) {

    var violationFromScratch = 0
    val violationArrayFromScratch = Array.fill(nbTask)(0)

    for(taskID <- taskIndices){
      val curStart = start(taskID).value
      val curDuration = duration(taskID).value
      val curEnd = curStart + curDuration

      for(otherTaskID <- taskIndices if taskID < otherTaskID){
        val otherStart = start(otherTaskID).value
        val otherDuration = duration(otherTaskID).value
        val otherEnd = otherStart + otherDuration
        val overlap = math.max(0,
          math.min(
            math.min(otherEnd-curStart,curEnd-otherStart),
            math.min(curDuration,otherDuration)))
        violationArrayFromScratch(taskID) += overlap
        violationArrayFromScratch(otherTaskID) += overlap
        violationFromScratch += overlap
      }
    }

    c.check(violation.value == violationFromScratch)
    for(t <- taskIndices){
      c.check(violation(start(t)).value == violationArrayFromScratch(t))
      c.check(violation(duration(t)).value == violationArrayFromScratch(t))
    }

    val nonZeroTasksFromScratch = SortedSet.empty[Int] ++ taskIndices.filter(i => duration(i).value != 0)
    c.check(nonZeroTasksFromScratch equals nonZeroTasks)

  }
}
