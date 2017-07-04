package oscar.cbls.business.routing.model.newModelStructure

import oscar.cbls.algo.search.KSmallest

import scala.math.min

/**
  * Created by fg on 4/07/17.
  */
object ClosestNeighbors {

  def computeClosestNeighborsForward(n: Int,
                                     getDistance: (Int,Int) => Int,
                                     filter : ((Int,Int) => Boolean) = (_,_) => true):Array[Iterable[Int]] = {

    def arrayOfAllNodes = Array.range(0,n-1)
    Array.tabulate(n)(node =>
      KSmallest.lazySort(arrayOfAllNodes.filter(filter(node,_)),
        neighbor => getDistance(node, neighbor)
      ))
  }

  def computeClosestNeighborsForwardOneValueFilter(n: Int,
                                                   getDistance: (Int,Int) => Int,
                                                   filter : ((Int) => Boolean) = _ => true):Array[Iterable[Int]] = {

    def arrayOfAllNodes = Array.range(0,n-1)
    Array.tabulate(n)(node =>
      KSmallest.lazySort(arrayOfAllNodes.filter(filter(_)),
        neighbor => getDistance(node, neighbor)
      ))
  }

  def computeClosestNeighborsBackward(n: Int,
                                      getDistance: (Int,Int) => Int): Array[Iterable[Int]] = {

    def arrayOfAllNodes = Array.range(0,n-1)
    Array.tabulate(n)(node =>
      KSmallest.lazySort(arrayOfAllNodes,
        neighbor => getDistance(neighbor, node)
      ))
  }

  def computeClosestNeighborsMinFWBW(n: Int,
                                     getDistance: (Int,Int) => Int): Array[Iterable[Int]] = {

    def arrayOfAllNodes = Array.range(0,n-1)
    Array.tabulate(n)(node =>
      KSmallest.lazySort(arrayOfAllNodes,
        neighbor => min(getDistance(neighbor, node), getDistance(node, neighbor))
      ))
  }

  /**
    * Filters the node itself and unreachable neighbors.
    */
  def reachableNeigbors(n: Int,
                        getDistance: (Int,Int) => Int,
                        node: Int) =
    (0 until n).toArray.filter((node2: Int) =>
      node != node2
        && (getDistance(node, node2) != Int.MaxValue
        || getDistance(node2, node) != Int.MaxValue)).toList

  /**
    * Returns the k nearest nodes of a given node.
    * It allows us to add a filter (optional) on the neighbor.
    *
    * Info : it uses the Currying feature.
    *
    * @param k the parameter k.
    * @param filter the filter.
    * @param node the given node.
    * @return the k nearest neighbor as an iterable list of Int.
    */
  def kFirst(k: Int, values:Array[Iterable[Int]], filter: (Int => Boolean) = _ => true)(node: Int): Iterable[Int] = {
    KSmallest.kFirst(Math.min(k,values(node).size): Int, values(node), filter)
  }

}
