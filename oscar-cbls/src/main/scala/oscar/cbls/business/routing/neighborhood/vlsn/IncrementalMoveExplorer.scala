package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.Objective
import oscar.cbls.core.search.{Neighborhood, _}

abstract sealed class CachedExploration
case class CachedAtomicMove(move:Move,delta:Int) extends CachedExploration
case object CachedAtomicNoMove extends CachedExploration
case object CacheDirty extends CachedExploration

object CachedAtomicMove{
  def apply(edge:Edge) = new CachedAtomicMove(edge.move,edge.deltaObj)
}

import oscar.cbls.business.routing.neighborhood.vlsn.VLSNMoveType._

import scala.collection.immutable.{SortedMap, SortedSet}


object CachedExplorations{
  def apply(oldGraph:VLSNGraph,
            performedMoves:List[Edge],
            v:Int):Option[CachedExplorations] = {

    var dirtyNodes: SortedSet[Int] = SortedSet.empty
    val isDirtyVehicle = Array.fill[Boolean](v)(false)

    for (edge: Edge <- performedMoves) {
      val fromNode = edge.from
      val toNode = edge.to

      edge.moveType match {
        case InsertNoEject =>
          dirtyNodes += fromNode.representedNode
          isDirtyVehicle(toNode.vehicle) = true
        case InsertWithEject =>
          dirtyNodes += fromNode.representedNode
          isDirtyVehicle(toNode.vehicle) = true
        case MoveNoEject =>
          isDirtyVehicle(fromNode.vehicle) = true
          isDirtyVehicle(toNode.vehicle) = true
        case MoveWithEject =>
          isDirtyVehicle(fromNode.vehicle) = true
          isDirtyVehicle(toNode.vehicle) = true
        case Remove =>
          isDirtyVehicle(fromNode.vehicle) = true
          dirtyNodes += fromNode.representedNode
        case Symbolic => ;
      }
    }

    //println("isDirtyVehicle:" + isDirtyVehicle.mkString(","))
    //println(oldGraph.statistics)

    if(isDirtyVehicle.forall(p => p)) None
    else Some(new CachedExplorations(oldGraph: VLSNGraph,
      dirtyNodes:SortedSet[Int],
        isDirtyVehicle: Array[Boolean],
      v: Int))
  }
}

class CachedExplorations(oldGraph:VLSNGraph,
                         dirtyNodes:SortedSet[Int], //only for unrouted nodes that were inserted of newly removed
                         isDirtyVehicle:Array[Boolean],
                         v:Int) {

  def isDirtyNode(node: Int): Boolean = dirtyNodes.contains(node)

  //TODO: use arrays for O(1) access?
  var cachedInsertNoEject: SortedMap[(Int, Int), CachedAtomicMove] = SortedMap.empty //unroute,targetVehicle
  var cachedInsertWithEject: SortedMap[(Int, Int), CachedAtomicMove] = SortedMap.empty //
  var cachedMoveNoEject: SortedMap[(Int, Int), CachedAtomicMove] = SortedMap.empty //node,vehicle
  var cachedMoveWithEject: SortedMap[(Int, Int), CachedAtomicMove] = SortedMap.empty
  var cachedRemove: SortedMap[(Int), CachedAtomicMove] = SortedMap.empty

  var size = 0

  for(fromNode <- oldGraph.nodes){
    val vehicleOfFromNode = fromNode.vehicle
    if((vehicleOfFromNode == v) || (vehicleOfFromNode >= 0 && !isDirtyVehicle(vehicleOfFromNode))){

      for (edge <- fromNode.outgoing){
       require(edge.from == fromNode)
        val toNode = edge.to

        edge.moveType match {
          case InsertNoEject if !isDirtyNode(fromNode.representedNode) && !isDirtyVehicle(toNode.vehicle) =>
            cachedInsertNoEject += (fromNode.representedNode, toNode.vehicle) -> CachedAtomicMove(edge)
            size+=1
          case InsertWithEject if !isDirtyNode(fromNode.representedNode) && !isDirtyVehicle(toNode.vehicle) =>
            cachedInsertWithEject += (fromNode.representedNode, toNode.representedNode) -> CachedAtomicMove(edge)
            size+=1
          case MoveNoEject if !isDirtyVehicle(fromNode.vehicle) && !isDirtyVehicle(toNode.vehicle) =>
            cachedMoveNoEject += (fromNode.representedNode, toNode.vehicle) -> CachedAtomicMove(edge)
            size+=1
          case MoveWithEject if !isDirtyVehicle(fromNode.vehicle) && !isDirtyVehicle(toNode.vehicle) =>
            cachedMoveWithEject += (fromNode.representedNode, toNode.representedNode) -> CachedAtomicMove(edge)
            size+=1
          case Remove if !isDirtyVehicle(fromNode.vehicle) =>
            cachedRemove += fromNode.representedNode -> CachedAtomicMove(edge)
            size+=1
          case Symbolic => ;
          case _ => ; // non cachable
        }
      }
    }
  }

  //println("cache size:" + size)

  def getInsertOnVehicleNoRemove(unroutedNodeToInsert: Int,
                                 targetVehicleForInsertion: Int): CachedExploration = {
    if (!isDirtyNode(unroutedNodeToInsert) && !isDirtyVehicle(targetVehicleForInsertion)) {
      cachedInsertNoEject.getOrElse((unroutedNodeToInsert, targetVehicleForInsertion), CachedAtomicNoMove)
    } else {
      CacheDirty
    }
  }

  def getInsertOnVehicleWithRemove(unroutedNodeToInsert: Int,
                                   targetVehicleForInsertion: Int,
                                   removedNode: Int): CachedExploration = {
    if (!isDirtyNode(unroutedNodeToInsert) && !isDirtyVehicle(targetVehicleForInsertion)) {
      cachedInsertWithEject.getOrElse((unroutedNodeToInsert, removedNode), CachedAtomicNoMove)
    } else {
      CacheDirty
    }
  }

  def getMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle: Int, targetVehicle: Int): CachedExploration = {
    if (!isDirtyVehicle(fromVehicle) && !isDirtyVehicle(targetVehicle)) {
      cachedMoveNoEject.getOrElse((routingNodeToMove, targetVehicle), CachedAtomicNoMove)
    } else {
      CacheDirty
    }
  }

  def getMoveToVehicleWithRemove(routingNodeToMove: Int, fromVehicle: Int, targetVehicle: Int, removedNode: Int): CachedExploration = {
    if (!isDirtyVehicle(fromVehicle) && !isDirtyVehicle(targetVehicle)) {
      cachedMoveWithEject.getOrElse((routingNodeToMove, removedNode), CachedAtomicNoMove)
    } else {
      CacheDirty
    }
  }

  def getRemoveNode(removedNode: Int, fromVehicle: Int): CachedExploration = {
    if (!isDirtyVehicle(fromVehicle)) {
      cachedRemove.getOrElse(removedNode, CachedAtomicNoMove)
    } else {
      CacheDirty
    }
  }
}

class IncrementalMoveExplorerAlgo(v:Int,
                                  vehicleToRoutedNodes:SortedMap[Int,Iterable[Int]],
                                  unroutedNodesToInsert:Iterable[Int],
                                  nodeToRelevantVehicles:Map[Int,Iterable[Int]],

                                  targetVehicleNodeToInsertNeighborhood:Int => Int => Neighborhood,
                                  targetVehicleNodeToMoveNeighborhood:Int => Int => Neighborhood,
                                  nodeToRemoveNeighborhood:Int => Neighborhood,

                                  removeAndReInsert:Int => () => Unit,
                                  useDirectInsert:Boolean,

                                  vehicleToObjectives:Array[Objective],
                                  unroutedNodesPenalty:Objective,
                                  globalObjective:Objective,

                                  cached:CachedExplorations
                                 )
  extends MoveExplorerAlgo(v:Int,
    vehicleToRoutedNodes:SortedMap[Int,Iterable[Int]],
    unroutedNodesToInsert:Iterable[Int],
    nodeToRelevantVehicles:Map[Int,Iterable[Int]],

    targetVehicleNodeToInsertNeighborhood:Int => Int => Neighborhood,
    targetVehicleNodeToMoveNeighborhood:Int => Int => Neighborhood,
    nodeToRemoveNeighborhood:Int => Neighborhood,

    removeAndReInsert:Int => () => Unit,
    useDirectInsert,

    vehicleToObjectives:Array[Objective],
    unroutedNodesPenalty:Objective,
    globalObjective:Objective){


  override def evaluateInsertOnVehicleNoRemove(unroutedNodeToInsert: Int, targetVehicleForInsertion: Int): (Move, Int) = {
    cached.getInsertOnVehicleNoRemove(unroutedNodeToInsert,targetVehicleForInsertion) match{
      case CachedAtomicMove(move:Move,delta:Int) =>
        assert(super.evaluateInsertOnVehicleNoRemove(unroutedNodeToInsert, targetVehicleForInsertion)._2 == delta)
        (move,delta)
      case CachedAtomicNoMove =>
        assert(super.evaluateInsertOnVehicleNoRemove(unroutedNodeToInsert, targetVehicleForInsertion) == null)
        null
      case CacheDirty =>
        super.evaluateInsertOnVehicleNoRemove(unroutedNodeToInsert, targetVehicleForInsertion)
    }
  }

  override def evaluateInsertOnVehicleWithRemove(unroutedNodeToInsert: Int, targetVehicleForInsertion: Int, removedNode: Int, correctedGlobalInit: Int): (Move, Int) = {
    cached.getInsertOnVehicleWithRemove(unroutedNodeToInsert,targetVehicleForInsertion,removedNode) match {
      case CachedAtomicMove(move: Move, delta: Int) =>
        assert(super.evaluateInsertOnVehicleWithRemove(unroutedNodeToInsert, targetVehicleForInsertion, removedNode, correctedGlobalInit)._2 == delta)
        (move, delta)
      case CachedAtomicNoMove =>
        assert(super.evaluateInsertOnVehicleWithRemove(unroutedNodeToInsert, targetVehicleForInsertion, removedNode, correctedGlobalInit) == null)
        null
      case CacheDirty =>
        super.evaluateInsertOnVehicleWithRemove(unroutedNodeToInsert, targetVehicleForInsertion, removedNode, correctedGlobalInit)
    }
  }

  override def evaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle:Int, targetVehicle: Int): (Move, Int) = {
    cached.getMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle: Int, targetVehicle: Int) match{
      case CachedAtomicMove(move: Move, delta: Int) =>
        assert(super.evaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle:Int, targetVehicle: Int)._2 == delta)
        (move, delta)
      case CachedAtomicNoMove =>
        assert(super.evaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle:Int, targetVehicle: Int) == null,
          s"evaluateMoveToVehicleNoRemove(routingNodeToMove:$routingNodeToMove, fromVehicle:$fromVehicle, targetVehicle:$targetVehicle) super:" +
            super.evaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle:Int, targetVehicle: Int))
        null
      case CacheDirty =>
        super.evaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle:Int, targetVehicle: Int)
    }
  }

  override def evaluateMoveToVehicleWithRemove(routingNodeToMove: Int, fromVehicle: Int, targetVehicleID: Int, removedNode: Int): (Move, Int) = {
    cached.getMoveToVehicleWithRemove(routingNodeToMove, fromVehicle, targetVehicleID, removedNode) match{
      case CachedAtomicMove(move: Move, delta: Int) =>
        assert(super.evaluateMoveToVehicleWithRemove(routingNodeToMove, fromVehicle, targetVehicleID, removedNode)._2 == delta)
        (move, delta)
      case CachedAtomicNoMove =>
        assert(super.evaluateMoveToVehicleWithRemove(routingNodeToMove, fromVehicle, targetVehicleID, removedNode) == null)
        null
      case CacheDirty =>
        super.evaluateMoveToVehicleWithRemove(routingNodeToMove, fromVehicle, targetVehicleID, removedNode)
    }
  }

  override def evaluateRemove(routingNodeToRemove: Int, fromVehicle: Int): (Move, Int) = {
    cached.getRemoveNode(routingNodeToRemove,fromVehicle) match{
      case CachedAtomicMove(move: Move, delta: Int) =>
        assert(super.evaluateRemove(routingNodeToRemove, fromVehicle)._2 == delta)
        (move, delta)
      case CachedAtomicNoMove =>
        assert(super.evaluateRemove(routingNodeToRemove, fromVehicle) == null)
        null
      case CacheDirty =>
        super.evaluateRemove(routingNodeToRemove, fromVehicle)
    }
  }
}
