package oscar.cbls.business.routing.model

import oscar.cbls.business.routing.model.extensions.VRPExtension
import oscar.cbls._
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing._
import oscar.cbls.lib.invariant.seq.Content
import oscar.cbls.lib.invariant.set.Diff

import scala.collection.immutable.{List, SortedSet}

/**
  * Created by fg on 12/09/17.
  */
class VRP(val m: Store, val n: Int, val v: Int,
          val maxPivotPerValuePercent: Int = 4){

  var vRPExtensions: QList[VRPExtension] = null

  val routes = new CBLSSeqVar(m, IntSequence(0 until v), n-1, "routes", maxPivotPerValuePercent=maxPivotPerValuePercent)

  /**
    * the range of nodes (customers and deposits including) of the problem.
    */
  val nodes = 0 until n

  /**
    * the range vehicle of the problem.
    */
  val vehicles = 0 until v

  val vehicleOfNode = vehicleOfNodes(routes,v)

  val routed = Content(routes.createClone(50)).setName("routed nodes")
  val unrouted = Diff(CBLSSetConst(SortedSet(nodes:_*)),routed).setName("unrouted nodes")

  m.registerForPartialPropagation(unrouted)

  def isADepot(node: Int) = node < v

  def kFirst(k: Int, values:(Int) => Iterable[Int], filter: Int => Int => Boolean = _ => _ => true)(node: Int): Iterable[Int] = {
    if (k >= n - 1) return values(node).filter(filter(node))

    KSmallest.kFirst(k: Int, values(node), filter(node))
  }

  /**
    * Returns if a given point is still routed.
    *
    * @param n the point queried.
    * @return true if the point is still routed, else false.
    */
  def isRouted(n: Int): Boolean = {routed.value.contains(n)}

  /**
    * This function is intended to be used for testing only.
    * setCircuit(List(1,2,3,4)) produces the following route :
    * 1 -> 2 -> 3 -> 4 (-> 1)
    */
  def setCircuit(nodes: Iterable[Int]): Unit = {
    routes := IntSequence(nodes)
    for(v <- 0 until v) require(routes.value.contains(v))
  }

  def unroutedNodes:Iterable[Int] = nodes.filterNot(isRouted)

  def onSameVehicle()(node1:Int,node2:Int): Boolean = {
    vehicleOfNode(node1) == vehicleOfNode(node2) && isRouted(node1)
  }

  def onVehicle(vehicle:Int)(node:Int): Boolean={
    vehicleOfNode(node).value == vehicle
  }

  /**
    * the route of the vehicle, starting at the vehicle node, and not including the last vehicle node
    *
    * @param vehicle
    * @return
    */
  def getRouteOfVehicle(vehicle:Int):List[Int] = {
    require(vehicle < v)
    var currentVExplorer = routes.value.explorerAtAnyOccurrence(vehicle).head.next
    var acc:List[Int] = List(vehicle)
    while (currentVExplorer match{
      case Some(x) if x.value >= v =>
        acc = x.value :: acc
        currentVExplorer = x.next
        true
      case _ => false}) {}
    acc.reverse
  }

  def getRoutePositionOfAllNode:Array[Int] = {
    def buildRoutePositionOfAllNode(it: Iterator[Int],currentPosition: Int, nodeToPosition: List[Int]): Array[Int] = {
      if(!it.hasNext)
        nodeToPosition.toArray
      else{
        val node = it.next()
        if(node < v)
          buildRoutePositionOfAllNode(it,0,nodeToPosition ++ List(0))
        else
          buildRoutePositionOfAllNode(it,currentPosition+1,nodeToPosition ++ List(currentPosition))

      }
    }
    val it = routes.value.iterator
    buildRoutePositionOfAllNode(it,0,List.empty)
  }

  def addExtension(extension: VRPExtension): Unit ={
    vRPExtensions = QList(extension,vRPExtensions)
  }

  def generatePostFilters(additionnalFilter: (Int) => Boolean = _ => true)(node:Int): (Int) => Boolean = {
    val filters: List[(Int) => Boolean] = List(additionnalFilter) ++ vRPExtensions.map(_.postFilter(node))
    def filterAll(neighbor: Int): Boolean ={
      val filtersIterator = filters.toIterator
      var isNeighborValid = true
      while(filtersIterator.hasNext && isNeighborValid) {
        if (!filtersIterator.next()(neighbor))
          isNeighborValid = false
      }
      return isNeighborValid
    }

    filterAll
  }

  lazy val preComputedRelevantNeighborsOfNodes: Array[List[Int]] ={
    val nodesList = nodes.toList
    def preComputeRelevantNeighborsOfNode(node: Int): List[Int] = {
      var relevantNeighbors: List[Int] = nodesList
      for (extension <- vRPExtensions)
        relevantNeighbors = extension.preComputeRelevantNeighborsOfNode(node, relevantNeighbors)
      relevantNeighbors
    }
    Array.tabulate(n)(node => if(node < v) List.empty else preComputeRelevantNeighborsOfNode(node))
  }

  /**
    * Redefine the toString method.
    * @return the VRP problem as a String.
    */
  override def toString: String = {
    var toReturn = ""
    var notMoving:List[Int] = List.empty

    for (vehicle <- 0 until v) {
      val routeOfV = getRouteOfVehicle(vehicle)
      if(routeOfV.length == 1){
        notMoving  = vehicle :: notMoving
      }else{
        toReturn +=  "vehicle " + vehicle + ": " +  routeOfV.mkString("->") + "->" + vehicle + "\n"
      }
    }
    "Vehicle routing n:" + n + " v:" + v + "\n" +
      "unrouted nodes:{" + unroutedNodes.toList.mkString(",") + "}\n" +
      "not used vehicles:{" + notMoving.reverse.mkString(",") + "}\n" +
      toReturn
  }
}
