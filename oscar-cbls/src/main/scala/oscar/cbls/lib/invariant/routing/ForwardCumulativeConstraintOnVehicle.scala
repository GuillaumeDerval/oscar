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

import oscar.cbls.algo.magicArray.MagicIntArrayStacked
import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.routing.convention.VehicleLocation


object ForwardCumulativeConstraintOnVehicle {
  /**
   * the violation maintained by this invariant is the sum over all routed nodes of the overshoot
   * strictly above cMax and the undershoot strictly below 0 of the content of the vehicles
   * @param routes
   * @param n The maximum number of nodes
   * @param v The number of vehicles
   * @param op A function which describes the capacity between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
   * @param cMax the maximal capacity of all vehicles (it is shared among all vehicles, so if you do not like it, you can use contentAtVehicleStart to make up for this)
   * @param contentAtVehicleStart the content of the vehicle at its start point
   * @param maxCheckpointLevel the maximal level of checkpoints that this should support.
   *                           it consumes O(n) memory per level, so do not overdrive uselessly
   * @param maxStack the max levels of stack used for some internal data structure related to vehicle start.
   *                 if you use lin-kerninghan-like neighborhood, or other circle-mode exploration,
   *                 you might want to set it to some relatively small value, compared to the number of vehicles, such as 1%
   */
  def apply (routes:ChangingSeqValue,
             n:Int,
             v:Int,
             op :(Int,Int,Int)=>Int,
             cMax:Int,
             contentAtVehicleStart:Array[Int],
             maxCheckpointLevel:Int,
             maxStack:Int,
             capacityName:String):ChangingIntValue = {

    val violation = new CBLSIntVar(routes.model, 0, 0 to Int.MaxValue, "violation of capacity " + capacityName)

    new ForwardCumulativeConstraintOnVehicle(
      routes,
      n,
      v,
      op,
      cMax,
      contentAtVehicleStart,
      violation,
      maxCheckpointLevel,
      maxStack)

    violation
  }
}


/**
 * the violation maintained by this invariant is the sum over all routed nodes of the overshoot
 * strictly above cMax and the undershoot strictly below 0 of the content of the vehicles
 * @param routes
 * @param n The maximum number of nodes
 * @param v The number of vehicles
 * @param op A function which describes the capacity between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
 * @param cMax the maximal capacity of all vehicles (it is shared among all vehicles, so if you do not like it, you can use contentAtVehicleStart to make up for this)
 * @param contentAtVehicleStart the content of the vehicle at its start point
 * @param violation the violation that will be controled by the invariant
 * @param maxCheckpointLevel the maximal level of checkpoints that this should support.
 *                           it consumes O(n) memory per level, so do not overdrive uselessly
 * @param maxStack the max levels of stack used for some internal data structure related to vehicle start.
 *                 if you use lin-kerninghan-like neighborhood, or circle-mode exploration,
 *                 you might want to set it to some relatively small value, compared to the number of vehicles, such as 1%
 */
class ForwardCumulativeConstraintOnVehicle(routes:ChangingSeqValue,
                                           n:Int,
                                           v:Int,
                                           op :(Int,Int,Int)=>Int,
                                           cMax:Int,
                                           contentAtVehicleStart:Array[Int],
                                           violation:CBLSIntVar,
                                           maxCheckpointLevel:Int,
                                           maxStack:Int)
  extends AbstractVehicleCapacity(n,v,op) with SeqNotificationTarget {
  require(contentAtVehicleStart.length==v)
  require(cMax >=0,"cMax should be >=0")
  require(contentAtVehicleStart.forall(_ <= cMax),"cannot exceed cMax in intitial values (ok this is because implementer was lazy, just remplace violation :=0 by violation := sum(contentToViolation(initValue))")

  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  violation.setDefiningInvariant(this)

  violation :=0

  private val contentAtNode = new MagicIntArrayStacked(maxCheckpointLevel, _ => 0, n)
  private val violationAndVehicleStartStack = new SeqCheckpointedValueStack[(Int,VehicleLocation)]()
  private var currentVehicleLocation:VehicleLocation = computeAndAffectContentAndVehicleStartPositionsFromScratch(routes.value,false)

  override def getContentAtVehicleStart(vehicle : Int) : Int = contentAtVehicleStart(vehicle)

  override def setEndNodeOfVehicle(vehicle : Int, lastNode : Int){}

  override def setVehicleContentAtEnd(vehicle : Int, content : Int){}

  override def getVehicleContentAtNode(node : Int) : Int = contentAtNode(node)

  override def setVehicleContentAtNode(node: Int, newContentAtNode: Int) {
    val oldContent = contentAtNode(node)
    contentAtNode(node) = newContentAtNode
    violation :+= (contentToViolation(newContentAtNode) - contentToViolation(oldContent))
  }

  override def setNodesUnrouted(unroutedNodes : Iterable[Int]){
    for(node <- unroutedNodes){
      violation :-= contentToViolation(getVehicleContentAtNode(node))
      contentAtNode(node) = 0
    }
  }

  @inline
  private def contentToViolation(content:Int):Int = {
    if (content < 0) - content
    else {
      val overshoot = content - cMax
      if (overshoot > 0) overshoot
      else 0
    }
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate){
    val (toUpdateZonesAndVehicleStartAfter,potentiallyRemovedNodes) =
      digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(changes,Some(RedBlackTreeMap.empty[List[(Int,Int)]],currentVehicleLocation),List.empty,v.value)

    setNodesUnrouted(potentiallyRemovedNodes)

    toUpdateZonesAndVehicleStartAfter match{
      case Some((vehiclesToZonesToUpdate,vehicleLocation)) =>
        updateVehicleContentOnAllVehicle(changes.newValue,
          vehiclesToZonesToUpdate,
          vehicleLocation)
        currentVehicleLocation = vehicleLocation
      case None =>
        currentVehicleLocation = computeAndAffectContentAndVehicleStartPositionsFromScratch(changes.newValue,false)
    }
  }

  def digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(changes:SeqUpdate,
                                                                       toUpdateZonesAndVehiceStartOpt:Option[(RedBlackTreeMap[List[(Int,Int)]],VehicleLocation)],
                                                                       potentiallyRemovedPoints:List[Int],
                                                                       previousSequence:IntSequence)
  :(Option[(RedBlackTreeMap[List[(Int,Int)]],VehicleLocation)],List[Int]) = {

    changes match {
      case s@SeqUpdateInsert(value : Int, posOfInsert : Int, prev : SeqUpdate) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence) match {
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), potentiallyRemovedPointsAfterPrev) =>
            val vehicleLocationAfterInsert = vehicleLocationAfterPrev.push(s.oldPosToNewPos,maxStack)
            val updatedZones =
              updateZoneToUpdateAfterInsert(
                zonesAfterPrev,
                posOfInsert,
                prev.newValue,
                vehicleLocationAfterPrev,
                vehicleLocationAfterInsert)
            (Some((updatedZones, vehicleLocationAfterInsert)), potentiallyRemovedPointsAfterPrev)
          case (None,potentiallyRemovedPointsAfterPrev) =>
            (None, potentiallyRemovedPointsAfterPrev)
        }

      case r@SeqUpdateRemove(pos : Int, prev : SeqUpdate) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence) match {
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), potentiallyRemovedPointsAfterPrev) =>
            val updatedZones =
              updateZoneToUpdateAfterRemove(
                zonesAfterPrev,
                pos : Int,
                prev.newValue, vehicleLocationAfterPrev)
            (Some((updatedZones, vehicleLocationAfterPrev.push(r.oldPosToNewPos,maxStack))), r.removedValue :: potentiallyRemovedPointsAfterPrev)
          case (None,potentiallyRemovedPointsAfterPrev) =>
            (None, r.removedValue :: potentiallyRemovedPointsAfterPrev)
        }

      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence) match {
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), potentiallyRemovedPointsAfterPrev) =>
            val vehicleLocationAfterMove = vehicleLocationAfterPrev.push(m.oldPosToNewPos,maxStack)
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
        (toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints)

      case s@SeqUpdateDefineCheckpoint(prev : SeqUpdate, isStarMode:Boolean, checkpointLevel:Int) =>
        if(checkpointLevel < maxCheckpointLevel) {
          digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence) match {
            //checkpoints are managed about the vehicleLocation exclusively
            case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), removedPointsAfterPrev) =>

              while (contentAtNode.level >= checkpointLevel) {
                //we save the changes, so they are comitted to the lower level
                contentAtNode.popLevel(false)
              }
              contentAtNode.pushLevel()

              val regularizedVehicleLocation = vehicleLocationAfterPrev.regularize
              setNodesUnrouted(potentiallyRemovedPoints)
              updateVehicleContentOnAllVehicle(prev.newValue,
                zonesAfterPrev,
                regularizedVehicleLocation)

              currentVehicleLocation = regularizedVehicleLocation
              violationAndVehicleStartStack.defineCheckpoint(prev.newValue, checkpointLevel, (violation.newValue, regularizedVehicleLocation))

              (Some(RedBlackTreeMap.empty[List[(Int, Int)]], currentVehicleLocation), List.empty)

            case (None, potentiallyRemovedPointsAfterPrev) =>

              while (contentAtNode.level >= checkpointLevel) {
                //we do not save the changes because we cannot perform incremental computation anyway
                contentAtNode.popLevel(true)
              }
              contentAtNode.pushLevel()
              setNodesUnrouted(v until n)
              violation :=0
              //we have to set all unrouted nodes to unrouted, since we have lost continuity on routes nodes because of the popLevel(true) hereabove
              currentVehicleLocation = computeAndAffectContentAndVehicleStartPositionsFromScratch(routes.value, false)
              violationAndVehicleStartStack.defineCheckpoint(prev.newValue, checkpointLevel, (violation.newValue, currentVehicleLocation))

              (Some(RedBlackTreeMap.empty[List[(Int, Int)]], currentVehicleLocation), List.empty)
          }
        }else{
          //we are above the maxCheckpoint level
          digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence)
        }
      case u@SeqUpdateRollBackToCheckpoint(checkpoint : IntSequence, level:Int) =>
        if(level <= maxCheckpointLevel){

          while (contentAtNode.level >= level) {
            contentAtNode.popLevel(true)
          }
          val (violation,vehicleLocation) = violationAndVehicleStartStack.rollBackAndOutputValue(checkpoint, level)
          currentVehicleLocation = vehicleLocation
          this.violation := violation
          (Some(RedBlackTreeMap.empty[List[(Int, Int)]], currentVehicleLocation), List.empty)
        }else{
          //We are above the max checkpoint level
          digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(u.howToRollBack, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence)
          //TODO: we could save and restore the regularized vehicle start, but his is probably not useful
        }
    }
  }

  override def checkInternals(c: Checker): Unit = {
    val (nodeToContent,_,vehicleStartPos) = computeNodeToContentAndVehicleContentAtEndAndVehicleStartPositionsFromScratch(routes.value, 0)
    for(node <- routes.value){
      c.check(nodeToContent(node) equals getVehicleContentAtNode(node),
        Some("GenericCumulativeConstraint : Error on content at node(" + node + ") at pos : "+ routes.newValue.positionsOfValue(node)+ " :=" + getVehicleContentAtNode(node) + " should be :=" + nodeToContent(node) + " route:" + routes.value))
    }
    val computedViolation = nodeToContent.foldLeft(0)({case (acc,nodeContent) => acc + contentToViolation(nodeContent)})
    c.check(computedViolation == violation.value, Some("GenericCumulativeConstraint : " + violation + " should be :="+computedViolation))
    for(node <- 0 until n){
      if(routes.value.contains(node)){
        c.check(nodeToContent(node) == getVehicleContentAtNode(node),Some("Error on content of routed node " + node))
      }else{
        c.check(nodeToContent(node) == 0 ,Some("Error on computed content of unrouted node " + node))
        c.check(getVehicleContentAtNode(node) == 0 ,Some("Error on content of unrouted node " + node + " is " + getVehicleContentAtNode(node) + " should be 0"))
      }
    }

    for(vehicle <- 0 until v) {
      c.check(routes.value.valueAtPosition(vehicleStartPos.startPosOfVehicle(vehicle)).get == vehicle,Some("a"))
      c.check(routes.value.valueAtPosition(currentVehicleLocation.startPosOfVehicle(vehicle)).get == vehicle,
        Some("routes.value.valueAtPosition(currentVehicleLocation.startPosOfVehicle(" + vehicle + ")) is" + routes.value.valueAtPosition(currentVehicleLocation.startPosOfVehicle(vehicle)) + " should be " + vehicle))
    }
  }
}

