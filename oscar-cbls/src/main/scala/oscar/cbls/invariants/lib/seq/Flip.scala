package oscar.cbls.invariants.lib.seq

import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.{ErrorChecker, Checker}

/**
 * maintains this as the flipped value of v
 * @param v
 * @param maxPivotPerValuePercent
 * @param maxHistorySize
 *                       THIS IS EXPERIMENTAL!
 */
case class Flip(v: SeqValue,override val maxPivotPerValuePercent:Int = 10, override val maxHistorySize:Int = 10)
  extends SeqInvariant(v.value.flip(true,true), v.max, maxPivotPerValuePercent, maxHistorySize)
  with SeqNotificationTarget{

  setName("Flip(" + v.name + ")")

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    if (!digestChanges(changes)) {
      this := changes.newValue.flip(true,true)
    }
  }

  val checkpointStack = new SeqCheckpointedValueStack[IntSequence]()

  def digestChanges(changes : SeqUpdate) : Boolean = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false

        //build on the original value instead of maintaining two data structs? , changes.newValue.flip(fast=true)
        this.insertAtPosition(value, prev.newValue.size - pos)

        true

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        val prevSize = prev.newValue.size-1
        println("digesting " + changes)

        val tentativeFlippedAfter = prevSize - after - 2
        val flippedFromIncluded = prevSize - toIncluded - 1
        val flippedToIncluded = prevSize - fromIncluded - 1
        val flippedAfter = if(tentativeFlippedAfter == flippedToIncluded) flippedFromIncluded - 1 else tentativeFlippedAfter
        this.move(flippedFromIncluded, flippedToIncluded, flippedAfter, flip)
        true

      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.remove(prev.newValue.size - position - 1,changes.newValue.flip(fast=true))
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
        false
    }
  }

  override def checkInternals(c: Checker) {
    println(this.newValue,v.value.size)
    c.check(this.newValue.toList equals v.value.toList.reverse, Some("this.newValue(=" + this.newValue.toList + ") == v.value.flip(=" + v.value.toList.reverse + ")"))
    c.check(this.newValue.toList.reverse equals v.value.toList, Some("this.newValue.flip(="+ this.newValue.toList.reverse +") == v.value(="+ v.value.toList+ ")"))
  }
}

