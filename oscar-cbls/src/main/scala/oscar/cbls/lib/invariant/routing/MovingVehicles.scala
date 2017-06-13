package oscar.cbls.lib.invariant.routing

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.functional.{IntSequenceExplorer, IntSequence}
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.routing.convention.RoutingConventionMethods

import scala.collection.immutable.SortedSet

class MovingVehicles(routes:ChangingSeqValue, v:Int)
  extends SetInvariant() with SeqNotificationTarget{

  registerStaticAndDynamicDependency(routes)
  finishInitialization()

  private val savedValues:Array[SortedSet[Int]] = null
  private var savedCheckpoint:IntSequence = null

  affect(computeValueFromScratch(routes.value))

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate){
    if(!digestUpdates(changes)) {
      dropCheckpoint()
      affect(computeValueFromScratch(changes.newValue))
    }
  }

  private def digestUpdates(changes:SeqUpdate):Boolean = {
    val newValue = changes.newValue

    changes match {
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we insert?
        if(!digestUpdates(prev)) return false
        val insertedVehicle = RoutingConventionMethods.searchVehicleReachingPosition(pos,newValue,v)
        nodesOfVehicleOrUnrouted(insertedVehicle) :+= value
        nodesOfVehicleOrUnrouted(v) :-= value
        recordMovedPoint(value, v, insertedVehicle)

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
          val targetVehicleOfMove = RoutingConventionMethods.searchVehicleReachingPosition(after,oldValue,v)
          if(vehicleOfMovedSegment != targetVehicleOfMove){
            //we moved all the points to another vehicle
            for(movedValue <- x.movedValuesQList) {
              nodesOfVehicleOrUnrouted(vehicleOfMovedSegment) :-= movedValue
              nodesOfVehicleOrUnrouted(targetVehicleOfMove) :+= movedValue
              recordMovedPoint(movedValue, vehicleOfMovedSegment, targetVehicleOfMove)
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
        nodesOfVehicleOrUnrouted(impactedVehicle) :-= removedValue
        nodesOfVehicleOrUnrouted(v) :+= removedValue
        recordMovedPoint(removedValue, impactedVehicle, v)
        true
      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
      case SeqUpdateLastNotified(value:IntSequence) =>
        true //we are starting from the previous value
      case SeqUpdateDefineCheckpoint(prev,isStarMode,checkpointLevel) =>
        if(checkpointLevel == 0) {
          if (!digestUpdates(prev)) {
            affect(computeValueFromScratch(changes.newValue))
          }
          saveCurrentCheckpoint(prev.newValue)
          true
        }else{
          //we do not handle other checkpoint, so ignore declaration
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
      nodesOfVehicleOrUnrouted(vehicleOfNodeAfterMoveForMovedPoints(node)) :-= node
      nodesOfVehicleOrUnrouted(vehicleOfNodeAtCheckpointForMovedPoints(node)) :+= node
    }
  }

  private def recordMovedPoint(node:Int, oldVehicle:Int, newVehicle:Int){
    require(oldVehicle != newVehicle)
    if(savedCheckpoint!= null) {
      if (!movedNodesSinceCheckpointArray(node)) {
        movedNodesSinceCheckpointList = QList(node, movedNodesSinceCheckpointList)
        movedNodesSinceCheckpointArray(node) = true
        vehicleOfNodeAtCheckpointForMovedPoints(node) = oldVehicle
      }
      vehicleOfNodeAfterMoveForMovedPoints(node) = newVehicle
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
    val values = computeValueFromScratch(routes.value)
    for (vehicle <- 0 to v){
      c.check(nodesOfVehicleOrUnrouted(vehicle).value equals values(vehicle), Some("error on vehicle " + v + " output-correct:" + (nodesOfVehicleOrUnrouted(vehicle).value.diff(values(vehicle))) + " correct-output:" + (values(vehicle).diff(nodesOfVehicleOrUnrouted(vehicle).value))))
    }

    if(savedCheckpoint != null) {
      val nodesOfVehicleFromScratch = computeValueFromScratch(savedCheckpoint)
      for (node <- 0 to n-1) {
        if(movedNodesSinceCheckpointArray(node))
          c.check(nodesOfVehicleFromScratch(vehicleOfNodeAtCheckpointForMovedPoints(node)).contains(node))
      }
    }
  }
}
