/*package oscar.cbls.lib.invariant.routing

import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.core.computation._
import oscar.cbls.lib.invariant.routing.convention.VehicleLocation

/**
 * This invariant has no output, only a method to get the vehicle reaching a given position, and a method to get the stat point of a vehicle
 */
class StartPointOfVehicles(routes:ChangingSeqValue,
                           v:Int)
  extends Invariant() with SeqNotificationTarget {

  private val vehicleStartStack = new SeqCheckpointedValueStack[VehicleLocation]()
  private var currentVehicleLocation:VehicleLocation = VehicleLocation(v,routes.value.positionOfAnyOccurrence(_).get)

  def startPosOfVehicle(vehicle:Int):Int = {
    currentVehicleLocation.startPosOfVehicle(vehicle)
  }

  def vehicleReachingPosition(position:Int):Int = {
    currentVehicleLocation.vehicleReachingPosition(position)
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate){
    currentVehicleLocation = digestUpdates(changes)
  }

  def digestUpdates(changes:SeqUpdate):VehicleLocation = {
    changes match {
      case s@SeqUpdateInsert(value : Int, posOfInsert : Int, prev : SeqUpdate) =>
        digestUpdates(prev).push(s.oldPosToNewPos)

      case r@SeqUpdateRemove(pos : Int, prev : SeqUpdate) =>
        digestUpdates(prev).push(r.oldPosToNewPos)

      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        digestUpdates(prev).push(m.oldPosToNewPos)

      case SeqUpdateAssign(value) =>
        VehicleLocation(v,value.positionOfAnyOccurrence(_).get)

      case SeqUpdateLastNotified(value) =>
        currentVehicleLocation

      case s@SeqUpdateDefineCheckpoint(prev : SeqUpdate, isStarMode:Boolean, checkpointLevel:Int) =>
        val peviousVehicleStart = if(checkpointLevel == 0) digestUpdates(prev).regularize else digestUpdates(prev)
        vehicleStartStack.defineCheckpoint(prev.newValue, checkpointLevel, peviousVehicleStart)

      case u@SeqUpdateRollBackToCheckpoint(checkpoint : IntSequence, level:Int) =>


          val (violation,vehicleLocation) = violationAndVehicleStartStack.rollBackAndOutputValue(checkpoint, level)
          currentVehicleLocation = vehicleLocation

    }


  }
*/

