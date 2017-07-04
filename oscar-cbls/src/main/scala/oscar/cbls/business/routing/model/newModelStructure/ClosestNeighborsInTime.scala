package oscar.cbls.business.routing.model.newModelStructure

import oscar.cbls.algo.seq.functional.IntSequenceExplorer

import scala.collection.immutable.List
import scala.math.max
/*
/**
  * Created by fg on 4/07/17.
  */
object ClosestNeighborsInTime {

  /**
    * This method compute the closest neighbor of a node base on arrivalTime.
    * @param k  the max number of closestNeighbor we want to inspect
    * @param filter an undefined filter used to filter the neighbor (neighbor,node) => Boolean
    * @param node the node we want to find neighbor for
    * @return the k closest neighbor of the node
    */
  def computeClosestNeighborsInTime(k: Int = Int.MaxValue,
                                    filter: (Int,Int) => Boolean = (_,_) => true
                                   )(node:Int): Iterable[Int] ={
    def buildPotentialNeighbors(explorer: Option[IntSequenceExplorer], potentialNeighbors: List[Int]): List[Int] = {
      if (explorer.isEmpty)
        potentialNeighbors
      else if (explorer.get.value < v && !availableVehicles.value.contains(explorer.get.value))
        buildPotentialNeighbors(explorer.get.prev, potentialNeighbors)
      else
        buildPotentialNeighbors(explorer.get.prev, List(explorer.get.value) ++ potentialNeighbors)
    }
    val explorer = sortedRouteByEarlylines.positionOfSmallestGreaterOrEqual(node)(deadlines(node))
    val potentialNeighbors = (
      if(explorer.isDefined) buildPotentialNeighbors(explorer,List.empty)
      else availableVehicles.value.toList.map(x => prev(x).value)).
      filter(prevNode => if(prevNode < v) vehiclesMaxCapacities(prevNode) >= contentsFlow(node) else true)
    buildClosestNeighbor(
      node,
      potentialNeighbors,  // arrival time of a node is 0 by default.
      filter,
      List.empty[(Int,Int)]
    ).take(k)
  }

  /**
    *
    * @param neighbors
    * @param closestNeighbors
    * @return
    */
  private def buildClosestNeighbor(node: Int,
                                   neighbors: List[Int],
                                   filter: (Int,Int) => Boolean = (_,_) => true,
                                   closestNeighbors: List[(Int,Int)]): List[Int] ={
    if(neighbors.isEmpty)
      return closestNeighbors.sortBy(_._2).map(_._1)
    val neighbor = neighbors.head
    if (filter(neighbor,node)  &&
      leaveTimes(neighbor).value + travelDurationMatrix.getTravelDuration(neighbor, 0, node) <= deadlines(node)) {
      val nextOfNeighbor = next(neighbor).value
      val neighborToNode = max(leaveTimes(neighbor).value + travelDurationMatrix.getTravelDuration(neighbor, 0, node), earlylines(node))
      val neighborToNodeToNext = neighborToNode + taskDurations(node) + travelDurationMatrix.getTravelDuration(node, 0, nextOfNeighbor)
      if (neighborToNodeToNext <= deadlines(nextOfNeighbor))
        return buildClosestNeighbor(node, neighbors.tail, filter, List((neighbor,neighborToNodeToNext)) ++ closestNeighbors)
    }
    buildClosestNeighbor(node, neighbors.tail, filter, closestNeighbors)
  }

  /**
    * This method compute the closest neighbor of a node base on arrivalTime.
    * It filter the clusters to avoid checking neighbor belonging to cluster
    * before the prevNode's cluster or after nextNode's cluster.
    * @param k  the max number of closestNeighbor we want to inspect
    * @param routeOfNode The route we want to find closest neighbor within
    *                    If you use this method many times, try to get the route once and for all
    *                    in the calling method.
    * @param filter an undefined filter used to filter the neighbor (neighbor,node) => Boolean
    * @param node the node we want to find neighbor for
    * @return the k closest neighbor of the node
    */
  def computeClosestNeighborsInTimeInRoute(k: Int = Int.MaxValue,
                                           routeOfNode: Option[List[Int]],
                                           filter: (Int,Int) => Boolean = (_,_) => true)(node:Int): Iterable[Int] ={
    val pickup = getRelatedPickup(node)
    val pickupEarlyLine = earlylines(pickup)
    val neighbors = routeOfNode.getOrElse(getRouteOfVehicle(getVehicleOfNode(node))).dropWhile(leaveTimes(_).value < pickupEarlyLine)
    buildClosestNeighbor(node, neighbors, filter, List.empty).take(k)
  }

}
*/