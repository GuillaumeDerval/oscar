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

import oscar.cbls._
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.core._

/**
 * maintains this as the flipped value of v
 * @param v
 * @param maxPivotPerValuePercent
 * @param maxHistorySize
 */
case class Flip(v: SeqValue,override val maxPivotPerValuePercent:Long = 10L, override val maxHistorySize:Long = 10L)
  extends SeqInvariant(v.value.flip(true,true), v.max, maxPivotPerValuePercent, maxHistorySize)
  with SeqNotificationTarget{

  setName("Flip(" + v.name + ")")

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Long, changes: SeqUpdate) {
    if (!digestChanges(changes)) {
      this := changes.newValue.flip(true,true)
    }
  }

  val checkpointStack = new SeqCheckpointedValueStack[IntSequence]()

  def digestChanges(changes : SeqUpdate) : Boolean = {
    changes match {
      case s@SeqUpdateInsert(value : Long, pos : Long, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false

        //build on the original value instead of maintaining two data structs? , changes.newValue.flip(fast=true)
        this.insertAtPosition(value, prev.newValue.size - pos)

        true

      case m@SeqUpdateMove(fromIncluded : Long, toIncluded : Long, after : Long, flip : Boolean, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        if(m.isNop) {
          ;
        }else if(m.isSimpleFlip){
          this.flip(0L,prev.newValue.size -1L)
        }else {
          //Complete move with a flip

          //there is a special case if the after is -1L
          if (after == -1L){
            //the segment to move starts at zero, and ends later
            val numberOfMovesPointsMinusOne = toIncluded - fromIncluded
            val prevSize = prev.newValue.size
            val flippedFromIncluded = prevSize - toIncluded - 1L
            val flippedToIncluded = prevSize - fromIncluded - 1L
            this.move(flippedFromIncluded, flippedToIncluded, prev.newValue.size-1L, flip)

          }else {
            val prevSize = prev.newValue.size
            val tentativeFlippedAfter = prevSize - after - 2L
            val flippedFromIncluded = prevSize - toIncluded - 1L
            val flippedToIncluded = prevSize - fromIncluded - 1L
            val flippedAfter = if (tentativeFlippedAfter == flippedToIncluded) flippedFromIncluded - 1L else tentativeFlippedAfter
            this.move (flippedFromIncluded, flippedToIncluded, flippedAfter, flip)
          }
        }
        true

      case r@SeqUpdateRemove(position : Long, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.remove(prev.newValue.size - position - 1L,changes.newValue.flip(fast=true))
        true

      case u@SeqUpdateRollBackToCheckpoint(checkPoint,level) =>
        releaseTopCheckpointsToLevel(level,false)
        this.rollbackToTopCheckpoint(checkpointStack.rollBackAndOutputValue(checkPoint,level))
        require(checkPoint quickEquals checkpointStack.topCheckpoint)
        assert(this.newValue quickEquals checkpointStack.outputAtTopCheckpoint(checkPoint))
        true

      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isStarMode, level) =>
        if(!digestChanges(prev)){
          this := changes.newValue.flip(false,true)
        }

        releaseTopCheckpointsToLevel(level,true)
        this.defineCurrentValueAsCheckpoint(isStarMode)
        checkpointStack.defineCheckpoint(prev.newValue,level,this.newValue)
        true

      case SeqUpdateLastNotified(value) =>
        true

      case SeqUpdateAssign(value : IntSequence) =>
        releaseTopCheckpointsToLevel(0L,true)
        false
    }
  }

  override def checkInternals(c: Checker) {
    c.check(this.newValue.toList equals v.value.toList.reverse, Some("this.newValue(=" + this.newValue.toList + ") == v.value.flip(=" + v.value.toList.reverse + ")"))
    c.check(this.newValue.toList.reverse equals v.value.toList, Some("this.newValue.flip(="+ this.newValue.toList.reverse +") == v.value(="+ v.value.toList+ ")"))
  }
}

