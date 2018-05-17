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
  def apply(oldGraph:VLSNGraph,performedMoves:List[Edge],v:Int):CachedExplorations = {

    var dirtyNodes: SortedSet[Int] = SortedSet.empty
    val isDirtyVehicle = Array.fill[Boolean](v)(false)

    for (edge: Edge <- oldGraph.edges) {
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
        case Remove if !isDirtyVehicle(fromNode.vehicle) =>
          isDirtyVehicle(fromNode.vehicle) = true
          dirtyNodes += fromNode.representedNode
        case Symbolic => ;
      }
    }

    new CachedExplorations(oldGraph: VLSNGraph,
      dirtyNodes: SortedSet[Int],
      isDirtyVehicle: Array[Boolean],
      v: Int)
  }
}

class CachedExplorations(oldGraph:VLSNGraph,
                         dirtyNodes:SortedSet[Int], //only for unrouted nodes that were inserted of newly removed
                         isDirtyVehicle:Array[Boolean],
                         v:Int) {

  def isDirtyNode(node: Int): Boolean = dirtyNodes.contains(node)

  var cachedInsertNoEject: SortedMap[(Int, Int), CachedAtomicMove] = SortedMap.empty //unroute,targetVehicle
  var cachedInsertWithEject: SortedMap[(Int, Int), CachedAtomicMove] = SortedMap.empty //
  var cachedMoveNoEject: SortedMap[(Int, Int), CachedAtomicMove] = SortedMap.empty
  var cachedMoveWithEject: SortedMap[(Int, Int), CachedAtomicMove] = SortedMap.empty
  var cachedRemove: SortedMap[(Int), CachedAtomicMove] = SortedMap.empty

  for (edge: Edge <- oldGraph.edges) {
    val fromNode = edge.from
    val toNode = edge.to

    edge.moveType match {
      case InsertNoEject if !isDirtyNode(fromNode.representedNode) && !isDirtyVehicle(toNode.vehicle) =>
        cachedInsertNoEject += (fromNode.representedNode, toNode.vehicle) -> CachedAtomicMove(edge)
      case InsertWithEject if !isDirtyNode(fromNode.representedNode) && !isDirtyVehicle(toNode.vehicle) =>
        cachedInsertWithEject += (fromNode.representedNode, toNode.representedNode) -> CachedAtomicMove(edge)
      case MoveNoEject if !isDirtyVehicle(fromNode.vehicle) && !isDirtyVehicle(toNode.vehicle) =>
        cachedMoveNoEject += (fromNode.representedNode, toNode.vehicle) -> CachedAtomicMove(edge)
      case MoveWithEject if !isDirtyVehicle(fromNode.vehicle) && !isDirtyVehicle(toNode.vehicle) =>
        cachedMoveWithEject += (fromNode.representedNode, toNode.representedNode) -> CachedAtomicMove(edge)
      case Remove if !isDirtyVehicle(fromNode.vehicle) =>
        cachedRemove += fromNode.representedNode -> CachedAtomicMove(edge)
      case Symbolic => ;
    }
  }

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

  def getMoveToVehicleNoRemove(fromVehicle: Int, routingNodeToMove: Int, targetVehicle: Int): CachedExploration = {
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

                                  nodeVehicleToInsertNeighborhood:(Int,Int) => Neighborhood,
                                  nodeTargetVehicleToMoveNeighborhood:(Int,Int) => Neighborhood,
                                  nodeToRemoveNeighborhood:Int => Neighborhood,

                                  removeAndReInsert:Int => () => Unit,

                                  vehicleToObjectives:Array[Objective],
                                  unroutedNodesPenalty:Objective,
                                  globalObjective:Objective,

                                  cached:CachedExplorations
                                 )
  extends MoveExplorerAlgo(v:Int,
    vehicleToRoutedNodes:SortedMap[Int,Iterable[Int]],
    unroutedNodesToInsert:Iterable[Int],
    nodeToRelevantVehicles:Map[Int,Iterable[Int]],

    nodeVehicleToInsertNeighborhood:(Int,Int) => Neighborhood,
    nodeTargetVehicleToMoveNeighborhood:(Int,Int) => Neighborhood,
    nodeToRemoveNeighborhood:Int => Neighborhood,

    removeAndReInsert:Int => () => Unit,

    vehicleToObjectives:Array[Objective],
    unroutedNodesPenalty:Objective,
    globalObjective:Objective){


  override def evaluateInsertOnVehicleNoRemove(unroutedNodeToInsert: Int, targetVehicleForInsertion: Int): (Move, Int) = {
    cached.getInsertOnVehicleNoRemove(unroutedNodeToInsert,targetVehicleForInsertion) match{
      case CachedAtomicMove(move:Move,delta:Int) => (move,delta)
      case CachedAtomicNoMove => null
      case CacheDirty =>
        super.evaluateInsertOnVehicleNoRemove(unroutedNodeToInsert, targetVehicleForInsertion)
    }
  }

  override def evaluateInsertOnVehicleWithRemove(unroutedNodeToInsert: Int, targetVehicleForInsertion: Int, removedNode: Int, correctedGlobalInit: Int): (Move, Int) = {
    cached.getInsertOnVehicleWithRemove(unroutedNodeToInsert,targetVehicleForInsertion,removedNode) match {
      case CachedAtomicMove(move: Move, delta: Int) => (move, delta)
      case CachedAtomicNoMove => null
      case CacheDirty =>
        super.evaluateInsertOnVehicleWithRemove(unroutedNodeToInsert, targetVehicleForInsertion, removedNode, correctedGlobalInit)
    }
  }

  override def evaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle:Int, targetVehicle: Int): (Move, Int) = {
    cached.getMoveToVehicleNoRemove(routingNodeToMove = routingNodeToMove, fromVehicle = fromVehicle, targetVehicle = targetVehicle) match{
      case CachedAtomicMove(move: Move, delta: Int) => (move, delta)
      case CachedAtomicNoMove => null
      case CacheDirty =>
        super.evaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle:Int, targetVehicle: Int)
    }
  }

  override def evaluateMoveToVehicleWithRemove(routingNodeToMove: Int, fromVehicle: Int, targetVehicleID: Int, removedNode: Int): (Move, Int) = {
    cached.getMoveToVehicleWithRemove(routingNodeToMove, fromVehicle, targetVehicleID, removedNode) match{
      case CachedAtomicMove(move: Move, delta: Int) => (move, delta)
      case CachedAtomicNoMove => null
      case CacheDirty =>
        super.evaluateMoveToVehicleWithRemove(routingNodeToMove, fromVehicle, targetVehicleID, removedNode)
    }
  }

  override def evaluateRemove(routingNodeToRemove: Int, fromVehicle: Int): (Move, Int) = {
    cached.getRemoveNode(routingNodeToRemove,fromVehicle) match{
      case CachedAtomicMove(move: Move, delta: Int) => (move, delta)
      case CachedAtomicNoMove => null
      case CacheDirty =>
        super.evaluateRemove(routingNodeToRemove, fromVehicle)
    }
  }
}
