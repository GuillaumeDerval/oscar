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
import oscar.cbls.core.computation.{ChangingSeqValue, SeqCheckpointedValueStack, SeqInvariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint, SeqValue}
import oscar.cbls.core.propagation.Checker

case class SortSequence(v: SeqValue,sortValue:Int => Int)
  extends SeqInvariant(IntSequence.empty(),v.max)
  with SeqNotificationTarget{

  setName("SortSequence(" + v.name + ")")

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  val checkpointStack = new SeqCheckpointedValueStack[IntSequence]()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    digestChanges(changes)
  }

  def searchPositionOfInsert(seq:IntSequence, value:Int):Int = {
    val transformedValue = sortValue(value)

    def otherIsSmaller(otherValue:Int):Boolean = {
      val otherTransformedValue = sortValue(otherValue)
      if(otherTransformedValue < transformedValue) true
      else if (otherTransformedValue > transformedValue) false
      else otherValue < value // tie break based on value because it must be deterministic.
    }

    if(seq.size == 0) return 0
    if(seq.size == 1) {
      if (otherIsSmaller(seq.head)) {
        return 0
      } else {
        return 1
      }
    }

    if(otherIsSmaller(seq.last)) return seq.size

    var lowerPositionOfInsert = 0
    var upperPositionOfInsert = seq.size-1

    while(lowerPositionOfInsert + 1 < upperPositionOfInsert) {
      val midPosition = (lowerPositionOfInsert + upperPositionOfInsert) / 2
      if (otherIsSmaller(seq.valueAtPosition(midPosition).get)) {
        lowerPositionOfInsert = midPosition
      } else {
        upperPositionOfInsert = midPosition
      }
    }
    lowerPositionOfInsert
  }

  def digestChanges(changes : SeqUpdate){
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        digestChanges(prev)
        //find where the value should be located by dichotomy
        val transformedPositionOfInsert = searchPositionOfInsert(prev.newValue, value)
        this.insertAtPosition(value,transformedPositionOfInsert)

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
    c.check(this.newValue.toList equals sortSequenceBy(v.value,sortValue), Some("this.newValue(=" + this.newValue.toList + ") == v.value.flip(=" + v.value.toList.reverse + ")"))
  }
}

