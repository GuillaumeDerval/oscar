package oscar.cbls.lib.invariant.routing

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

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.routing.convention.RoutingConventionMethods

object VehicleOfNodes{
  def apply(routes:ChangingSeqValue,v:Int):Array[CBLSIntVar] = {
    val model = routes.model
    val domain = routes.domain

    val vehicleOrUnroutedOfNode = Array.tabulate(routes.maxValue+1)((node:Int) =>
      CBLSIntVar(model,
        v,
        domain,
        "vehicle_or_unrouted_of_node_" + node))

    new VehicleOfNodes(routes, v, vehicleOrUnroutedOfNode)

    vehicleOrUnroutedOfNode
  }
}

/**
 * @param routes the routes of all the vehicles
 *
 * This invariant relies on the vehicle model assumption:
 * there are v vehicles
 * They are supposed to start from point of values 0 to v-1
 * These values must always be present in the sequence in increasing order
 * they cannot be included within a moved segment
 */
class VehicleOfNodes(routes:ChangingSeqValue,
                     v:Int,
                     vehicleOrUnroutedOfNode:Array[CBLSIntVar])
  extends Invariant() with SeqNotificationTarget{

  val n = routes.maxValue + 1

  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  vehicleOrUnroutedOfNode.foreach(_.setDefiningInvariant(this))

  private var savedCheckpoint:IntSequence = null
  //TODO: use magic array here
  private val movedNodesSinceCheckpointArray:Array[Boolean] = Array.fill(n)(false)
  private var movedNodesSinceCheckpointList:QList[Int] = null
  private val vehicleOfNodeAtCheckpointForMovedPoints:Array[Int] = Array.fill(n)(0)

  computeAndAffectValueFromScratch(routes.value)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate){
    if(!digestUpdates(changes)) {
      dropCheckpoint()
      computeAndAffectValueFromScratch(changes.newValue)
    }
  }

  private def digestUpdates(changes:SeqUpdate):Boolean = {
    val newValue = changes.newValue

    changes match {
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we insert?
        if(!digestUpdates(prev)) return false
        val insertedVehicle = RoutingConventionMethods.searchVehicleReachingPosition(pos,newValue,v)
        vehicleOrUnroutedOfNode(value) := insertedVehicle
        recordMovedPoint(value, v)

        true
      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){
          true
        }else {
          val oldValue = prev.newValue
          val vehicleOfMovedSegment = RoutingConventionMethods.searchVehicleReachingPosition(fromIncluded,oldValue,v)
          assert(vehicleOfMovedSegment == RoutingConventionMethods.searchVehicleReachingPosition(toIncluded,oldValue,v))
          val targetVehicleOfMove = RoutingConventionMethods.searchVehicleReachingPosition(after,oldValue,v)
          if(vehicleOfMovedSegment != targetVehicleOfMove){
            //we moved all the points to another vehicle
            for(movedValue <- x.movedValuesQList) {
              vehicleOrUnroutedOfNode(movedValue) := targetVehicleOfMove
              recordMovedPoint(movedValue, vehicleOfMovedSegment)
            }
          }
          true
        }

      case x@SeqUpdateRemove(position: Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        //on which vehicle did we insert?
        if(!digestUpdates(prev)) return false
        val oldValue = prev.newValue
        val impactedVehicle = RoutingConventionMethods.searchVehicleReachingPosition(position,oldValue,v)
        val removedValue = x.removedValue
        vehicleOrUnroutedOfNode(removedValue) := v
        recordMovedPoint(removedValue, impactedVehicle)
        true
      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
      case SeqUpdateLastNotified(value:IntSequence) =>
        true //we are starting from the previous value
      case SeqUpdateDefineCheckpoint(prev,isStarMode,checkpointLevel) =>
        if(checkpointLevel == 0) {
          if (!digestUpdates(prev)) {
            computeAndAffectValueFromScratch(changes.newValue)
          }
          saveCurrentCheckpoint(prev.newValue)
          true
        }else{
          digestUpdates(prev)
        }
      case r@SeqUpdateRollBackToCheckpoint(checkpoint,checkpointLevel) =>
        if(checkpoint == null) false //it has been dropped following a Set
        else {
          if(checkpointLevel == 0) {
            require(checkpoint quickEquals savedCheckpoint)
            restoreCheckpoint()
            true
          }else{
            digestUpdates(r.howToRollBack)
          }
        }
    }
  }

  private def dropCheckpoint(){
    saveCurrentCheckpoint(null)
  }
  private def saveCurrentCheckpoint(s:IntSequence){
    savedCheckpoint = s
    while (movedNodesSinceCheckpointList!= null) {
      movedNodesSinceCheckpointArray(movedNodesSinceCheckpointList.head) = false
      movedNodesSinceCheckpointList = movedNodesSinceCheckpointList.tail
    }
  }

  private def restoreCheckpoint(){
    while (movedNodesSinceCheckpointList!= null) {
      val node= movedNodesSinceCheckpointList.head
      movedNodesSinceCheckpointArray(movedNodesSinceCheckpointList.head) = false
      movedNodesSinceCheckpointList = movedNodesSinceCheckpointList.tail
      vehicleOrUnroutedOfNode(node) := vehicleOfNodeAtCheckpointForMovedPoints(node)
    }
  }

  private def recordMovedPoint(node:Int, oldVehicle:Int){
    if(savedCheckpoint!= null) {
      if (!movedNodesSinceCheckpointArray(node)) {
        movedNodesSinceCheckpointList = QList(node, movedNodesSinceCheckpointList)
        movedNodesSinceCheckpointArray(node) = true
        vehicleOfNodeAtCheckpointForMovedPoints(node) = oldVehicle
      }
    }
  }

  private def computeAndAffectValueFromScratch(s:IntSequence){
    vehicleOrUnroutedOfNode.foreach(_:=v) //unrouted

    val it = s.iterator
    var currentVehicle:Int = it.next()
    require(currentVehicle == 0)
    vehicleOrUnroutedOfNode(0) := 0

    while(it.hasNext){
      val node = it.next()
      if(node < v){
        //reaching a new vehicle start
        currentVehicle = node
      }
      //continuing on the same vehicle
      vehicleOrUnroutedOfNode(node) := currentVehicle
    }
  }

  private def computeValueFromScratch(s:IntSequence):Array[Int] = {
    val tmpVehicleOrUnroutedOfNode = Array.fill(n)(v)

    val it = s.iterator
    var currentVehicle:Int = it.next()
    require(currentVehicle == 0)
    tmpVehicleOrUnroutedOfNode(0) = 0

    while(it.hasNext){
      val node = it.next()
      if(node < v){
        //reaching a new vehicle start
        require(node == currentVehicle+1)
        currentVehicle = node
      }
      //continuing on the same vehicle
      tmpVehicleOrUnroutedOfNode(node) = currentVehicle
    }
    tmpVehicleOrUnroutedOfNode
  }

  override def checkInternals() : Unit = {
    val values = computeValueFromScratch(routes.value)
    for (node <- 0 to n-1){
      require(vehicleOrUnroutedOfNode(node).value == values(node), Some("vehicleOrUnroutedOfNode(node).value=" +vehicleOrUnroutedOfNode(node).value + " should== valuesFromScratch(node)=" + values(node) + " node:" + node))
    }

    if(savedCheckpoint != null) {
      val vehicleOfNodeFromScratch = computeValueFromScratch(savedCheckpoint)
      for (node <- 0 to n-1) {
        if(movedNodesSinceCheckpointArray(node)) {
          require(vehicleOfNodeFromScratch(node) == vehicleOfNodeAtCheckpointForMovedPoints(node))
        }
      }
    }
  }
}
