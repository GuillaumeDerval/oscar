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

package oscar.cbls.lib.invariant.routing

import oscar.cbls.algo.seq.functional.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

class MovingVehicles(routes:ChangingSeqValue, v:Int)
  extends SetInvariant() with SeqNotificationTarget{

  registerStaticAndDynamicDependency(routes)
  finishInitialization()

  this := computeValueFromScratch(routes.value)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate){
    if(!digestUpdates(changes)) {
      this := computeValueFromScratch(changes.newValue)
    }
  }

  private def digestUpdates(changes:SeqUpdate):Boolean = {

    changes match {
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we insert?
        if(!digestUpdates(prev)) return false

        require(pos != 0, "cannot insert at pos zero in routing")
        val prevValue = prev.newValue.valueAtPosition(pos-1).get
        if(prevValue < v && !this.newValue.contains(prevValue)){
          //prevValue is a vehicle that now goes out
          this.insertValue(prevValue)
        }

        true
      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){
          true
        }else {

          //chek what is before the moved segment; if it is a vehicle start, we should also check what is after the segment start
          val prevValueOfMovedSegment = prev.newValue.valueAtPosition(fromIncluded-1).get
          if(prevValueOfMovedSegment < v){
            assert(this.newValue.contains(prevValueOfMovedSegment))

            //We removed this segment from vehicle prevValueOfMovedSegment,
            // and it starts at vehicle start, so check that domething is left after the moved segment in this vehicle
            prev.newValue.valueAtPosition(fromIncluded+1) match{
              case None =>
                require(prevValueOfMovedSegment == v-1)
                assert(toIncluded == prev.newValue.size)
                this.deleteValue(prevValueOfMovedSegment)
              case Some(node) if node < v =>
                //we reach another vehicle, so there is nothing left in vehicle prevValueOfMovedSegment
                require(node == prevValueOfMovedSegment +1)
                this.deleteValue(prevValueOfMovedSegment)
              case _ =>
                ;
            }
          }

          //where is the segment inserted?
          val valueBeforeSegmentAterMove = prev.newValue.valueAtPosition(after).get
          if(valueBeforeSegmentAterMove < v && !this.newValue.contains(valueBeforeSegmentAterMove)){
            //prevValue is a vehicle that now goes out
            this.insertValue(valueBeforeSegmentAterMove)
          }
          true
        }

      case x@SeqUpdateRemove(position: Int, prev : SeqUpdate) =>
        if(!digestUpdates(prev)) return false

        require(position != 0, "cannot remove at pos zero in routing")

        val prevValue = prev.newValue.valueAtPosition(position-1).get
        if(prevValue < v) {
          val vehicleOfRemove = prevValue
          assert(this.newValue.contains(vehicleOfRemove))
          //prevValue is a vehicle that used to go out, and we remove its first node,
          // so we check that the node after the deleted one is not a vehicle start
          prev.newValue.valueAtPosition(position + 1) match {
            case None =>
              //we remove at the last position in the sequence,
              // so we removed the only point of the last vehicle
              require(vehicleOfRemove == v - 1)
              this.deleteValuePreviouslyIn(vehicleOfRemove)
            case Some(vehicleAfterRemove) if vehicleAfterRemove < v =>
              //the node after the remove is another vehicle start, so we removed the only node reached by the vehicle of remove
              require(vehicleAfterRemove == vehicleOfRemove + 1)
              this.deleteValuePreviouslyIn(vehicleOfRemove)
            case _ =>
            //nothing to do
          }
        }

        true

      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
      case SeqUpdateLastNotified(value:IntSequence) =>
        true //we are starting from the previous value
      case SeqUpdateDefineCheckpoint(prev,isStarMode,checkpointLevel) =>
        digestUpdates(prev)
      case r@SeqUpdateRollBackToCheckpoint(checkpoint,checkpointLevel) =>
            digestUpdates(r.howToRollBack)
    }
  }

  private def computeValueFromScratch(s:IntSequence):SortedSet[Int] = {
    var toReturn:SortedSet[Int] = SortedSet.empty
    var currentExplorer:IntSequenceExplorer = s.explorerAtPosition(0).get
    for(vehicle <- 0 until v){
      if(currentExplorer.value != vehicle){
        //instantiate an explorer because we do not have a proper one
        currentExplorer = s.explorerAtAnyOccurrence(vehicle).get
      }
      currentExplorer.next match{
        case None =>
          //we are at the last vehicle, and it does not move
          require(vehicle == v)
        case Some(e) if e.value != vehicle + 1 =>
          //there is a node after, and it is not hte next vehicle, so vehicle is moving
          toReturn += vehicle
        case Some(e) if e.value == vehicle + 1 =>
          //there is a node after, and it is the next vehicle, so vehicle is not moving
          //and we have an explorer at the next vehicle, so we save it for the next iteration
          currentExplorer = e
      }
    }
    toReturn
  }

  override def checkInternals(c : Checker) : Unit = {
    val valuesFromScratch = computeValueFromScratch(routes.value)
    c.check(valuesFromScratch equals this.newValue,
      Some("error on moving vehicle, got " + this.newValue.toList + " should be " + valuesFromScratch.toList))
  }
}

