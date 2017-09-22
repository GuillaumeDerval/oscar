package oscar.cbls.business.routing.model.extensions

import oscar.cbls.algo.search.KSmallest
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.computation.CBLSIntVar
import oscar.cbls.lib.invariant.numeric.Sum

import scala.collection.immutable.HashSet

/**
  * Created by fg on 14/09/17.
  */
class Distance(vrp: VRP, distanceMatrix: Array[Array[Int]], constantRoutingDistance: Array[CBLSIntVar]) extends VRPExtension(vrp){

  val totalDistance = Sum(constantRoutingDistance)
  val distancePerVehicle = constantRoutingDistance.map(_.value)

  private def getDistance(from: Int,to: Int) = distanceMatrix(from)(to)

  override def preComputeRelevantNeighborsOfNode(node: Int, potentialRelevantNeighbors: HashSet[Int]): HashSet[Int] = {
    potentialRelevantNeighbors
  }

  override def postFilter(node: Int) = (neighbor: Int) => true

  /**
    * FROM NODE TO NEIGHBORS
    *
    * This method returns a lazy sorted sequence of neighbors.
    * The neighbors are sorted considering the distance between a specified node and his neighbors.
    * We consider all neighbors in the array so you should filter them before calling this method.
    * @param node The node
    * @param neighbors An array of filtered neighbors
    */
  def computeClosestPathToNeighbor(neighbors: (Int) => Array[Int])(node:Int): Iterable[Int] ={
    KSmallest.lazySort(neighbors(node),
      neighbor => getDistance(node, neighbor)
    )
  }

  /**
    * FROM NEIGHBORS TO NODE
    *
    * This method returns a lazy sorted sequence of neighbors.
    * The neighbors are sorted considering the distance between a specified node and his neighbors
    * We consider all neighbors in the array so you should filter them before calling this method..
    * @param node The node
    * @param neighbors An array of filtered neighbors
    */
  def computeClosestPathFromNeighbor(neighbors: (Int) => HashSet[Int])(node: Int): Iterable[Int] ={
    KSmallest.lazySort(neighbors(node).toArray,
      neighbor => getDistance(neighbor, node)
    )
  }
}
