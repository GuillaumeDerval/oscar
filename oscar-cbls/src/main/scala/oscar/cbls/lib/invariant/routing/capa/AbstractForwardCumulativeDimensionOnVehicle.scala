package oscar.cbls.lib.invariant.routing.capa

import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.core.computation._
import oscar.cbls.lib.invariant.routing.AbstractVehicleCapacity
import oscar.cbls.lib.invariant.routing.convention.VehicleLocation


/**
 * creates a GenericCumulativeIntegerDimensionOnVehicle Invariant
 * @param routes The sequence representing the route associated at each vehicle
 * @param n The maximum number of nodes
 * @param v The number of vehicles
 */
abstract class AbstractForwardCumulativeDimensionOnVehicle(routes:ChangingSeqValue,
                                                           n:Int,
                                                           v:Int)
  extends AbstractVehicleCapacity(n,v)
  with SeqNotificationTarget with IntNotificationTarget{

  //the output is initialized here, together with the currentVehicleLocation
  protected var currentVehicleLocation:VehicleLocation =
    computeAndAffectContentAndVehicleStartPositionsFromScratch(routes.value,true)

  private val vehicleLocationAndCheckpointStack = new SeqCheckpointedValueStack[VehicleLocation]()

  protected var toUpdateZonesAndVehicleStartAfter:Option[(RedBlackTreeMap[List[(Int,Int)]],VehicleLocation)] =
    Some(RedBlackTreeMap.empty[List[(Int,Int)]],currentVehicleLocation)

  protected var potentiallyRemovedNodes:List[Int] = List.empty

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate){
    val tmp = digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(
      changes,
      toUpdateZonesAndVehicleStartAfter,
      potentiallyRemovedNodes,
      v.value)

    toUpdateZonesAndVehicleStartAfter = tmp._1
    potentiallyRemovedNodes = tmp._2
    scheduleForPropagation()
  }

  private def printToUpdateZonesAndVehicleStartAfter(toUpdateZonesAndVehicleStartAfter:Option[(RedBlackTreeMap[List[(Int,Int)]],VehicleLocation)]):String = {
    toUpdateZonesAndVehicleStartAfter match{
      case None => "None"
      case Some((a,b)) => "Some(" + a.content.map({case (a,l) => a + "->" + l}).mkString(",") + "," + b + ")"
    }
  }

  override def performPropagation(){
    setNodesUnrouted(potentiallyRemovedNodes)

    toUpdateZonesAndVehicleStartAfter match{
      case Some((vehiclesToZonesToUpdate,vehicleLocation)) =>

        updateVehicleContentOnAllVehicle(routes.value,
          vehiclesToZonesToUpdate,
          vehicleLocation)
        currentVehicleLocation = vehicleLocation
      case None =>
        currentVehicleLocation = computeAndAffectContentAndVehicleStartPositionsFromScratch(routes.value,false)
    }
    toUpdateZonesAndVehicleStartAfter = Some(RedBlackTreeMap.empty[List[(Int,Int)]],currentVehicleLocation)
    potentiallyRemovedNodes = List.empty
  }


  def digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(changes:SeqUpdate,
                                                                       toUpdateZonesAndVehicleStartOpt:Option[(RedBlackTreeMap[List[(Int,Int)]],VehicleLocation)],
                                                                       potentiallyRemovedPoints:List[Int],
                                                                       previousSequence:IntSequence)
  :(Option[(RedBlackTreeMap[List[(Int,Int)]],VehicleLocation)],List[Int]) = {

    changes match {
      case s@SeqUpdateInsert(value : Int, posOfInsert : Int, prev : SeqUpdate) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehicleStartOpt, potentiallyRemovedPoints, previousSequence) match {
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), potentiallyRemovedPointsAfterPrev) =>
            val vehicleLocationAfyterInsert = vehicleLocationAfterPrev.push(s.oldPosToNewPos)
            val updatedZones =
              updateZoneToUpdateAfterInsert(
                zonesAfterPrev,
                posOfInsert,
                prev.newValue,
                vehicleLocationAfterPrev,vehicleLocationAfyterInsert)
            (Some((updatedZones,  vehicleLocationAfyterInsert)), potentiallyRemovedPointsAfterPrev)
          case (None,potentiallyRemovedPointsAfterPrev) =>
            (None, potentiallyRemovedPointsAfterPrev)
        }

      case r@SeqUpdateRemove(pos : Int, prev : SeqUpdate) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehicleStartOpt, potentiallyRemovedPoints, previousSequence) match {
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), potentiallyRemovedPointsAfterPrev) =>
            val updatedZones =
              updateZoneToUpdateAfterRemove(
                zonesAfterPrev,
                pos : Int,
                prev.newValue, vehicleLocationAfterPrev)
            (Some((updatedZones, vehicleLocationAfterPrev.push(r.oldPosToNewPos))), r.removedValue :: potentiallyRemovedPointsAfterPrev)
          case (None,potentiallyRemovedPointsAfterPrev) =>
            (None, r.removedValue :: potentiallyRemovedPointsAfterPrev)
        }

      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehicleStartOpt, potentiallyRemovedPoints, previousSequence) match {
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), potentiallyRemovedPointsAfterPrev) =>
            val vehicleLocationAfterMove = vehicleLocationAfterPrev.push(m.oldPosToNewPos)
            val updatedZones =
              updateZoneToUpdateAfterMove(
                zonesAfterPrev,
                m,
                prev.newValue,
                vehicleLocationAfterPrev,
                vehicleLocationAfterMove)
            (Some((updatedZones, vehicleLocationAfterMove)), potentiallyRemovedPointsAfterPrev)

          case(None,potentiallyRemovedPointsAfterPrev) =>
            (None, potentiallyRemovedPointsAfterPrev)
        }

      case SeqUpdateAssign(value : IntSequence) =>
        (None, potentiallyRemovedPoints ::: previousSequence.unorderedContentNoDuplicate)

      case SeqUpdateLastNotified(value : IntSequence) =>
        (toUpdateZonesAndVehicleStartOpt, potentiallyRemovedPoints)

      case s@SeqUpdateDefineCheckpoint(prev : SeqUpdate, isStarMode:Boolean, checkpointLevel:Int) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehicleStartOpt, potentiallyRemovedPoints, previousSequence) match {
          //checkpoints are managed about the vehicleLocation exclusively
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), removedPointsAfterPrev) =>
            vehicleLocationAndCheckpointStack.defineCheckpoint(prev.newValue,checkpointLevel,vehicleLocationAfterPrev)
            (Some((zonesAfterPrev, vehicleLocationAfterPrev)), removedPointsAfterPrev)
          case (None,potentiallyRemovedPointsAfterPrev) =>
            (None, potentiallyRemovedPointsAfterPrev)
        }

      case u@SeqUpdateRollBackToCheckpoint(checkpoint : IntSequence, level:Int) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(u.howToRollBack,toUpdateZonesAndVehicleStartOpt, potentiallyRemovedPoints, previousSequence) match {
          //checkpoints are managed about the vehicleLocation exclusively
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), removedPointsAfterPrev) =>
            val regularizedVehicleLocation = vehicleLocationAndCheckpointStack.rollBackAndOutputValue(checkpoint,level)
            (Some((zonesAfterPrev, regularizedVehicleLocation)), removedPointsAfterPrev)
          case (None,potentiallyRemovedPointsAfterPrev) =>
            //in this case, we cannot exploit the regularized info
            //but this is a very strange case,actually
            vehicleLocationAndCheckpointStack.rollBackAndOutputValue(checkpoint,level)
            (None, potentiallyRemovedPointsAfterPrev)
        }
    }
  }
}

