package oscar.cbls.lib.invariant.seq

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

import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.core.computation.ChangingSeqValue
import oscar.cbls.core.computation.SeqCheckpointedValueStack
import oscar.cbls.core.computation.SeqInvariant
import oscar.cbls.core.computation.SeqNotificationTarget
import oscar.cbls.core.computation.SeqUpdate
import oscar.cbls.core.computation.SeqUpdateAssign
import oscar.cbls.core.computation.SeqUpdateDefineCheckpoint
import oscar.cbls.core.computation.SeqUpdateInsert
import oscar.cbls.core.computation.SeqUpdateLastNotified
import oscar.cbls.core.computation.SeqUpdateMove
import oscar.cbls.core.computation.SeqUpdateRemove
import oscar.cbls.core.computation.SeqUpdateRollBackToCheckpoint
import oscar.cbls.core.computation.SeqValue
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker
import oscar.cbls.core.propagation.Checker

/**
 * maintains this as the flipped value of v
 * @param v
 * @param maxPivotPerValuePercent
 * @param maxHistorySize
 */
case class SortSequence(v: SeqValue,sortValue:Int => Int)
  extends SeqInvariant(IntSequence.empty(),v.max)
  with SeqNotificationTarget{

  setName("Sort(" + v.name + ")")

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  val checkpointStack = new SeqCheckpointedValueStack[IntSequence]()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    digestChanges(changes)
  }

  def digestChanges(changes : SeqUpdate){
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        digestChanges(prev)

        //find where the value should be located by dychotomy
        



      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        digestChanges(prev)

      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        digestChanges(prev)

        val valueAtRemove = r.removedValue
        val posOfRemoveOut = this.newValue.positionOfAnyOccurrence(valueAtRemove).get
        this.remove(posOfRemoveOut)

      case u@SeqUpdateRollBackToCheckpoint(checkPoint,level) =>
        val outputCheckpoint = checkpointStack.rollBackAndOutputValue(checkPoint,level)
        this.releaseTopCheckpointsToLevel(level,false)
        this.rollbackToTopCheckpoint(outputCheckpoint)

      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isStarMode, level) =>
        digestChanges(prev)

        checkpointStack.defineCheckpoint(prev.newValue,level,this.newValue)
        this.releaseTopCheckpointsToLevel(level,true)
        this.defineCurrentValueAsCheckpoint(isStarMode)

      case SeqUpdateLastNotified(value) =>
      //nothing to do

      case SeqUpdateAssign(value : IntSequence) =>
        this := sortSequenceBy(value,sortValue)
    }
  }

  private def sortSequenceBy(i:IntSequence,by:Int => Int):IntSequence = IntSequence(i.toList.sortBy(by))

  override def checkInternals(c: Checker) {
    c.check(this.newValue.toList equals v.value.toList.reverse, Some("this.newValue(=" + this.newValue.toList + ") == v.value.flip(=" + v.value.toList.reverse + ")"))
    c.check(this.newValue.toList.reverse equals v.value.toList, Some("this.newValue.flip(="+ this.newValue.toList.reverse +") == v.value(="+ v.value.toList+ ")"))
  }
}

