package oscar.cbls.business.routing.invariants

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.VehicleLocation
import oscar.cbls.core._

/**
 * This invariant has no output, only a method to get the vehicle reaching a given position, and a method to get the stat point of a vehicle
  * it is intended to make neighborhoods simpler to implement
 * EXPERIMENTAL
 */
class StartPointOfVehicles(routes:ChangingSeqValue,
                           v:Long)
  extends Invariant() with SeqNotificationTarget {

  private val vehicleStartStack = new SeqCheckpointedValueStack[VehicleLocation]()
  private var currentVehicleLocation : VehicleLocation = VehicleLocation(v, routes.value.positionOfAnyOccurrence(_).get)

  def startPosOfVehicle(vehicle : Long) : Long = {
    currentVehicleLocation.startPosOfVehicle(vehicle)
  }

  def vehicleReachingPosition(position : Long) : Long = {
    currentVehicleLocation.vehicleReachingPosition(position)
  }

  def vehicleReachingNode(node: Long) : Option[Long] = {
    routes.value.positionOfAnyOccurrence(node) match{
      case Some(nodePosition) => Some(vehicleReachingPosition(nodePosition))
      case None => None
    }
  }

  def onSameVehicleOrBothUnrouted(node1:Long,node2:Long): Boolean={
    vehicleReachingNode(node1) == vehicleReachingNode(node2)
  }

  def onVehicle(vehicle:Long,node:Long): Boolean = {
    vehicleReachingNode(node) contains vehicle
  }

  override def notifySeqChanges(v : ChangingSeqValue, d : Long, changes : SeqUpdate) {
    currentVehicleLocation = digestUpdates(changes)
  }

  def digestUpdates(changes : SeqUpdate) : VehicleLocation = {
    changes match {
      case s@SeqUpdateInsert(value : Long, posOfInsert : Long, prev : SeqUpdate) =>
        digestUpdates(prev).push(s.oldPosToNewPos)

      case r@SeqUpdateRemove(pos : Long, prev : SeqUpdate) =>
        digestUpdates(prev).push(r.oldPosToNewPos)

      case m@SeqUpdateMove(fromIncluded : Long, toIncluded : Long, after : Long, flip : Boolean, prev : SeqUpdate) =>
        digestUpdates(prev).push(m.oldPosToNewPos)

      case SeqUpdateAssign(value) =>
        VehicleLocation(v, value.positionOfAnyOccurrence(_).get)

      case SeqUpdateLastNotified(value) =>
        currentVehicleLocation

      case s@SeqUpdateDefineCheckpoint(prev : SeqUpdate, isStarMode : Boolean, checkpointLevel : Long) =>
        val previousVehicleStart = if (checkpointLevel == 0L) digestUpdates(prev).regularize else digestUpdates(prev)
        vehicleStartStack.defineCheckpoint(prev.newValue, checkpointLevel, previousVehicleStart)
        previousVehicleStart

      case u@SeqUpdateRollBackToCheckpoint(checkpoint : IntSequence, level : Long) =>
        vehicleStartStack.rollBackAndOutputValue(checkpoint, level)
    }
  }

  override def checkInternals(c : Checker){
    currentVehicleLocation.checkOnSequence(routes.value)
  }
}
