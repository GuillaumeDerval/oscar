package oscar.cbls.business.routing.model.newModelStructure

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

import oscar.cbls.algo.search.KSmallest
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.core.computation._
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.invariant.routing._
import oscar.cbls.lib.invariant.routing.convention.RoutingConventionMethods
import oscar.cbls.lib.invariant.seq.{Content, Size}
import oscar.cbls.lib.invariant.set.Diff
import oscar.cbls.modeling.Algebra._
import oscar.cbls.visual.MatrixMap.RoutingMatrixContainer
import oscar.examples.cbls.routing.visual.ColorGenerator

import scala.collection.immutable.SortedSet
import scala.math._

/**
  * The class constructor models a VRP problem with N points (deposits and customers)
  * and V vehicles.
  *
  * Vehicles are supposed to leave from their depot, and come back to it.
  * they all have a different depot (but yo ucan put them at the same place if you want)
  *
  * Info: after instantiation, each customer point is unrouted, and each vehicle loop on his deposit.
  *
  * @param n the number of points (deposits and customers) in the problem.
  * @param v the number of vehicles.
  * @param m the model.
  * @author renaud.delandtsheer@cetic.be
  * @author Florent Ghilain (UMONS)
  */
class VRP(val n: Int, val v: Int, val m: Store, maxPivotPerValuePercent:Int = 4) {

  val routes = new CBLSSeqVar(m, IntSequence(0 until v), n-1, "routes", maxPivotPerValuePercent=maxPivotPerValuePercent)
  val nodesOfVehicle = NodeOfVehicle(routes,v)
  val (next,prev) = RouteSuccessorAndPredecessors(routes.createClone(),v,n)

  /**
    * the range of nodes (customers and deposits including) of the problem.
    */
  val nodes = 0 until n
  /**
    * the range vehicle of the problem.
    */
  val vehicles = 0 until v

  /**
    * Returns if a given point is a depot.
    *
    * @param n the point queried.
    * @return true if the point is a depot, else false.
    */
  def isADepot(n: Int): Boolean = { n < v }

  /**
    * Returns if a given point is still routed.
    *
    * @param n the point queried.
    * @return true if the point is still routed, else false.
    */
  def isRouted(n: Int): Boolean = {routes.value.contains(n)}

  /**
    * Returns if a given point is still routed.
    *
    * @param n the point queried.
    * @return true if the point is still routed, else false.
    */
  def isUnrouted(n: Int): Boolean = {!routes.value.contains(n)}

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

  /**
    * the route of the vehicle, starting at the vehicle node, and not including the last vehicle node
    *
    * @param vehicle
    * @return
    */
  def getRouteOfVehicle(vehicle:Int):List[Int] = {
    require(vehicle < v)
    nodesOfVehicle(vehicle).value.toList
  }

  def notOnSameVehicle(nodes: Iterable[Int], vehicle:Int): Iterable[Int]={
    nodes.filterNot(getVehicleOfNode(_) == vehicle)
  }

  def notOnSameVehicle(nodes: Array[Iterable[Int]])(vehicle:Int): Array[Iterable[Int]]={
    val resNodes = nodes.clone()
    for(i <- resNodes.indices){
      resNodes(i) = nodes(i).filterNot(getVehicleOfNode(_) == vehicle)
    }
    require(nodes != resNodes,"ERROR")
    resNodes
  }

  def onSameVehicle()(node1:Int,node2:Int): Boolean={
    getVehicleOfNode(node1)==getVehicleOfNode(node2)
  }

  def notOnSameVehicle()(node1:Int,node2:Int): Boolean={
    getVehicleOfNode(node1)!=getVehicleOfNode(node2)
  }

  def onVehicle(vehicle:Int)(node:Int): Boolean={
    getVehicleOfNode(vehicle)==getVehicleOfNode(node)
  }

  def notOnVehicle(vehicle:Int)(node:Int): Boolean={
    getVehicleOfNode(vehicle)!=getVehicleOfNode(node)
  }

  /**
    *
    * @param node a node
    * @return the vehicle reaching the node, v is it is unrouted
    */
  def getVehicleOfNode(node:Int):Int = {
    val routeValue = routes.value
    routeValue.positionOfAnyOccurrence(node) match{
      case None => v
      case Some(position) => RoutingConventionMethods.searchVehicleReachingPosition(position,routeValue,v)
    }
  }

  def getVehicleOfAllNodesFromScratch:Array[Int] = {
    val nodeToVehicle = Array.fill(n)(0)
    val it = routes.value.iterator
    var currentVehicle = it.next
    while(it.hasNext){
      val node = it.next()
      if(node < v){
        currentVehicle = node
      }
      nodeToVehicle(node) = currentVehicle
    }
    nodeToVehicle
  }

  def getRoutePositionOfAllNodeFromScratch:Array[Int] = {
    val routePosition = Array.fill(n)(0)
    val it = routes.value.iterator
    var currentRoutePosition = 0
    it.next
    while(it.hasNext){
      val node = it.next()
      if(node < v){
        currentRoutePosition = 0
      }else {
        currentRoutePosition += 1
      }
      routePosition(node) = currentRoutePosition
    }
    routePosition
  }

  /**
    * @return the route of a vehicle as a String.
    */
  def routeToString(vehicle: Int): String = {
    "vehicle " + vehicle + ": " + getRouteOfVehicle(vehicle).mkString("->")
  }
  /**
    * @return the list of unrouted nodes as a String.
    */
  def unroutedToString: String = {
    "unrouted nodes: " + unroutedNodes.toList + "\n"
  }

  /**
    * Redefine the toString method.
    * @return the VRP problem as a String.
    */
  override def toString: String = {
    var toReturn = ""
    var notMoving:List[Int] = List.empty

    for (vehicle <- 0 to v - 1) {
      val routeOfV = getRouteOfVehicle(vehicle)
      if(routeOfV.length == 1){
        notMoving  = vehicle :: notMoving
      }else{
        toReturn +=  "vehicle " + vehicle + ": " +  routeOfV.mkString("->") + "\n"
      }
    }
    "Vehicle routing n:" + n + " v:" + v + "\n" +
      "unrouted nodes: " + unroutedNodes.toList.mkString(",") + "\n" +
      "not used vehicles:" + notMoving.reverse.mkString(",") + "\n" +
      toReturn
  }
}


/**
  * Maintains the set of unrouted nodes.
  * Info : those whose next is N.
  *
  * @author renaud.delandtsheer@cetic.be
  * @author Florent Ghilain (UMONS)
  * @author yoann.guyot@cetic.be
  */
trait RoutedAndUnrouted extends VRP{
  /**
    * the data structure set which maintains the unrouted nodes.
    */
  val routed = Content(routes.createClone(50)).setName("routed nodes")
  val unrouted = Diff(CBLSSetConst(SortedSet(nodes:_*)),routed).setName("unrouted nodes")

  m.registerForPartialPropagation(unrouted)

  override def unroutedNodes : Iterable[Int] = unrouted.value
}

trait CloneOfRouteForLightPartialPropagation extends VRP{
  val cloneOfRoute = routes
}

trait NodesOfVehicle extends CloneOfRouteForLightPartialPropagation{
  override val nodesOfVehicle=NodeOfVehicle(cloneOfRoute,v)

  def getNodesOfVehicle(vehicle:Int):SortedSet[Int] = nodesOfVehicle(vehicle).value
}

trait VehicleOfNode extends CloneOfRouteForLightPartialPropagation{
  val vehicleOfNode = VehicleOfNodes(cloneOfRoute,v)

  override def getVehicleOfNode(node:Int):Int = vehicleOfNode(node).value

  override def isRouted(node: Int): Boolean = vehicleOfNode(node).value!=v
}

trait RoutingMapDisplay extends VRP{
  var routingMap:RoutingMatrixContainer = null

  /**
    * This method initialize the routing map of the problem
    * @param list this is the list of nodes that need to be drawn
    * @param mapSize the size of the map, do not specify it if you use the geoRoutingMap (it doesn't need this)
    * @param pickupAndDeliveryNodes if true, the map will show specific information about pickup and delivery nodes
    * @param geolocalisationMap if true, the geoRoutingMap will be used
    */
  def initializeRoutingMap(list:Array[(Double,Double)], vrp:VRP = this, mapSize: Int = 1000, pickupAndDeliveryNodes: Boolean = false, geolocalisationMap: Boolean = false, routeToDisplay: Boolean = false): Unit ={
    //routingMap = new RoutingMatrixContainer(title="toto",myVRP = vrp, pickupAndDeliveryPoints = pickupAndDeliveryNodes, geolocalisationMap = geolocalisationMap, routeToDisplay = routeToDisplay)
    routingMap.setMapSize(mapSize)
    routingMap.setPointsList(list.toList)
    routingMap.setColorValues(ColorGenerator.generateRandomColors(v))
    routingMap.drawPoints()
    new Thread(routingMap,"routing thread").start()
  }

  def drawRoutes(): Unit ={
    routingMap.allRoutes = Array.tabulate(v)(vehicle => getRouteOfVehicle(vehicle))
    routingMap.setMustRefresh(true)
  }
}


