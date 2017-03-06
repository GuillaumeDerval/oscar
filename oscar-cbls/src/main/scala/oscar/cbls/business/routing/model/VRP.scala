package oscar.cbls.business.routing.model

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

  /**unroutes all points of the VRP*/
  def unroute() {
    routes := IntSequence(0 until v)
  }

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

  /**
   * @return the list of unrouted nodes as a String.
   */
  def unroutedToString: String = {
    "unrouted: " + unroutedNodes.toList + "\n"
  }

  def unroutedNodes:Iterable[Int] = nodes.filterNot(isRouted)

  /**
   * @return the route of a vehicle as a String.
   */
  def routeToString(vehicle: Int): String = {
    "Vehicle " + vehicle + ": " + getRouteOfVehicle(vehicle).mkString("->")
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

  def getNodesOfVehicle(vehicle:Int):SortedSet[Int] = getNodesOfVehicleFromScratch(vehicle)

  def getNodesOfVehicleFromScratch(vehicle:Int):SortedSet[Int] = SortedSet.empty[Int] ++ getRouteOfVehicle(vehicle)

  /**
    * This method generate all the nodes preceding a specific position
    *
    * @param node the node
    * @return
    */
  def getNodesBeforePosition()(node:Int): List[Int] ={
    val position = routes.value.positionOfAnyOccurrence(node).head

    var i = v-1
    while(routes.value.explorerAtAnyOccurrence(i).head.position > position)
      i -= 1
    var currentVExplorer = routes.value.explorerAtAnyOccurrence(i).head.next
    var acc:List[Int] = List(i)
    while (currentVExplorer match{
      case Some(x) if x.position < position && x.value >= v =>
        acc = x.value :: acc
        currentVExplorer = x.next
        true
      case _ => false}) {}
    acc.reverse
  }

  /**
    * This method generate all the nodes following a specific position
    *
    * @param node the node
    * @return
    */
  def getNodesAfterNode()(node:Int): List[Int] ={
    val position = routes.value.positionOfAnyOccurrence(node).head

    var i = v-1
    while(routes.value.explorerAtAnyOccurrence(i).head.position > position)
      i -= 1

    var currentVExplorer = routes.value.explorerAtAnyOccurrence(node).head.next
    var acc:List[Int] = List(node,i)
    while (currentVExplorer match{
      case Some(x) if x.position >= position && x.value >= v =>
        acc = x.value :: acc
        currentVExplorer = x.next
        true
      case _ => false}) {}
    acc.reverse
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

  def getVehicleOfAllNodes:Array[Int] = {
    val nodeToVehicle = Array.fill(n)(v)
    val it = routes.value.iterator
    var currentVehicle = it.next
    nodeToVehicle(0) = 0
    while(it.hasNext){
      val node = it.next()
      if(node < v){
        currentVehicle = node
      }
      nodeToVehicle(node) = currentVehicle
    }
    nodeToVehicle
  }

  def getRoutePositionOfAllNode:Array[Int] = {
    val routePosition = Array.fill(n)(-1)
    val it = routes.value.iterator
    routePosition(0) = 0
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
   * Redefine the toString method.
    *
    * @return the VRP problem as a String.
   */
  override def toString: String = {
    var toReturn = unroutedToString

    for (v <- 0 to v - 1) {
      toReturn += routeToString(v)
      toReturn += "\n"
    }
    for (additionalStringFunction <- additionalStrings) {
      toReturn += additionalStringFunction() + "\n"
    }
    toReturn
  }

  private var additionalStrings: List[() => String] = List.empty
  protected def addToStringInfo(a: () => String) {
    additionalStrings = a :: additionalStrings
  }

  def onTheSameRoute(node1:Int,node2:Int):Boolean = getVehicleOfNode(node1) == getVehicleOfNode(node2)
}


trait NextAndPrev extends VRP{
  //TODO: ensure that we REALLY need such an expensive invariant, if yes, use it from the adequate trait, and use clone to speed up exploration!!!
  val (next,prev) = RouteSuccessorAndPredecessors(routes.createClone(),v,n)

}


trait ConstantDistancePerVehicle extends TotalConstantDistance{
  var distancePerVehicle:Array[CBLSIntVar] = null

  override def setSymmetricDistanceMatrix(symmetricDistanceMatrix:Array[Array[Int]],precomputeFW : Boolean = true, precomputeBW : Boolean = true){
    assert(ConstantRoutingDistance.isDistanceSymmetric(symmetricDistanceMatrix))
    this.distanceMatrix = symmetricDistanceMatrix
    this.matrixIsSymmetric = true
    require(distancePerVehicle == null)
    distancePerVehicle = ConstantRoutingDistance(routes, v ,true, symmetricDistanceMatrix, true, precomputeFW = true)
    totalDistance = Sum(distancePerVehicle)
  }

  override def setAsymmetricDistanceMatrix(asymetricDistanceMatrix:Array[Array[Int]],precomputeFW : Boolean = true, precomputeBW : Boolean = true){
    this.distanceMatrix = asymetricDistanceMatrix
    this.matrixIsSymmetric = false
    require(distancePerVehicle == null)
    distancePerVehicle = ConstantRoutingDistance(routes, v ,true, asymetricDistanceMatrix, false, precomputeFW, precomputeBW)
    totalDistance = Sum(distancePerVehicle)
  }
}

trait TotalConstantDistance extends VRP{
  var totalDistance:IntValue = null
  var distanceMatrix:Array[Array[Int]] = null
  var matrixIsSymmetric = false

  def setDistanceMatrix(distanceMatrix:Array[Array[Int]]){
    if(ConstantRoutingDistance.isDistanceSymmetric(distanceMatrix)){
      setSymmetricDistanceMatrix(distanceMatrix)
    }else{
      setAsymmetricDistanceMatrix(distanceMatrix)
    }
  }

  def setSymmetricDistanceMatrix(symmetricDistanceMatrix:Array[Array[Int]],precomputeFW : Boolean = true, precomputeBW : Boolean = true){
    require(totalDistance == null)
    assert(ConstantRoutingDistance.isDistanceSymmetric(symmetricDistanceMatrix))
    this.distanceMatrix = symmetricDistanceMatrix
    this.matrixIsSymmetric = true
    totalDistance = ConstantRoutingDistance(routes, v ,false, symmetricDistanceMatrix, true)(0)
  }

  def setAsymmetricDistanceMatrix(asymetricDistanceMatrix:Array[Array[Int]],precomputeFW : Boolean = true, precomputeBW : Boolean = true){
    require(totalDistance == null)
    this.distanceMatrix = asymetricDistanceMatrix
    this.matrixIsSymmetric = false
    totalDistance = ConstantRoutingDistance(routes, v ,false, asymetricDistanceMatrix, false,precomputeFW = true, precomputeBW = true)(0)
  }
}

/**
 * Computes the nearest neighbors of each point.
 * Used by some neighborhood searches.
  *
  * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
trait ClosestNeighbors extends VRP {

  protected def getDistance(from: Int, to: Int): Int

  def computeClosestNeighborsForward(filter : ((Int,Int) => Boolean) = (_,_) => true):Array[Iterable[Int]] = {
    def arrayOfAllNodes = Array.tabulate(n)(node => node)
    Array.tabulate(n)(node =>
      KSmallest.lazySort(arrayOfAllNodes.filter(filter(node,_)),
        neighbor => getDistance(node, neighbor)
      ))
  }

  def computeClosestNeighborsForwardOneValueFilter(filter : ((Int) => Boolean) = _ => true):Array[Iterable[Int]] = {
    def arrayOfAllNodes = Array.tabulate(n)(node => node)
    Array.tabulate(n)(node =>
      KSmallest.lazySort(arrayOfAllNodes.filter(filter(_)),
        neighbor => getDistance(node, neighbor)
      ))
  }

  def computeClosestNeighborsBackward(): Array[Iterable[Int]] = {
    def arrayOfAllNodes = Array.tabulate(n)(node => node)
    Array.tabulate(n)(node =>
      KSmallest.lazySort(arrayOfAllNodes,
        neighbor => getDistance(neighbor, node)
      ))
  }

  def computeClosestNeighborsMinFWBW(): Array[Iterable[Int]] = {
    def arrayOfAllNodes = Array.tabulate(n)(node => node)
    Array.tabulate(n)(node =>
      KSmallest.lazySort(arrayOfAllNodes,
        neighbor => min(getDistance(neighbor, node), getDistance(node, neighbor))
      ))
  }

  def computeClosestNeighborsOnRouteForward(vehicle:Int, filter : ((Int,Int) => Boolean) = (_,_) => true):Array[Iterable[Int]] = {
    def arrayOfAllNodes = Array.tabulate(n)(node => node)
    Array.tabulate(n)(node =>
      KSmallest.lazySort(arrayOfAllNodes.filter(getRouteOfVehicle(vehicle).contains(_)),
        neighbor => min(getDistance(neighbor, node), getDistance(node, neighbor))
      ))
  }

  /**
   * Filters the node itself and unreachable neighbors.
   */
  def reachableNeigbors(node: Int) =
    nodes.filter((node2: Int) =>
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
    if (k >= n - 1) return nodes.filter(filter)

    def kNearestAccumulator(sortedNeighbors: Iterator[Int], k: Int, kNearestAcc: List[Int]): List[Int] = {
      require(k >= 0)
      if(k == 0 || !sortedNeighbors.hasNext){
        kNearestAcc.reverse
      }else{
        val neighbor = sortedNeighbors.next()
        if (filter(neighbor))
          kNearestAccumulator(sortedNeighbors, k - 1, neighbor :: kNearestAcc)
        else
          kNearestAccumulator(sortedNeighbors, k, kNearestAcc)
      }
    }

    kNearestAccumulator(values(node).iterator, k, Nil)
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

trait AbstractPenaltyForUnrouted extends VRP{
  /**
   * the variable which maintains the sum of penalty of unrouted nodes, thanks to invariant SumElements.
   */
  var unroutedPenalty:ChangingIntValue=null
  addToStringInfo(() => ""+unroutedPenalty)
}

/**
 * Maintains and fixes a penalty weight of unrouted nodes.
  *
  * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
trait DetailedPenaltyForUnrouted extends AbstractPenaltyForUnrouted with RoutedAndUnrouted{
  def setDetailedUnroutedPenaltyWeights(penalties : Array[Int]) {
    require(unroutedPenalty == null)
    unroutedPenalty = Sum(penalties, unrouted).setName("TotalPenaltyForUnroutedNodes (detailed penalties)")
  }
}

trait StandardPenaltyForUnrouted extends AbstractPenaltyForUnrouted {
  def setStandardUnroutedPenaltyWeight(standardWeight:Int){
    require(unroutedPenalty == null)
    unroutedPenalty = (standardWeight * (n - Size(routes))).setName("TotalPenaltyForUnroutedNodes (standard penalties)")
  }
}

trait CloneOfRouteForLightPartialPropagation extends VRP{
  val cloneOfRoute = routes
}

trait NodesOfVehicle extends CloneOfRouteForLightPartialPropagation{
  val nodesOfVehicle=NodeOfVehicle(cloneOfRoute,v)

  override def getNodesOfVehicle(vehicle:Int):SortedSet[Int] = nodesOfVehicle(vehicle).value
}

trait VehicleOfNode extends CloneOfRouteForLightPartialPropagation{
  val vehicleOfNode = VehicleOfNodes(cloneOfRoute,v)

  override def getVehicleOfNode(node:Int):Int = vehicleOfNode(node).value

  override def isRouted(node: Int): Boolean = vehicleOfNode(node).value!=v

}

trait RoutingMapDisplay extends VRP with ConstantDistancePerVehicle{
  var routingMap:RoutingMatrixContainer = null

  /**
    * This method initialize the routing map of the problem
    * @param list this is the list of nodes that need to be drawn
    * @param mapSize the size of the map, do not specify it if you use the geoRoutingMap (it doesn't need this)
    * @param pickupAndDeliveryNodes if true, the map will show specific information about pickup and delivery nodes
    * @param geolocalisationMap if true, the geoRoutingMap will be used
    */
  def initializeRoutingMap(list:Array[(Double,Double)], vrp:VRP = this, mapSize: Int = 0, pickupAndDeliveryNodes: Boolean = false, geolocalisationMap: Boolean = false, routeToDisplay: Boolean = false): Unit ={
    routingMap = new RoutingMatrixContainer(myVRP = vrp, pickupAndDeliveryPoints = pickupAndDeliveryNodes, geolocalisationMap = geolocalisationMap, routeToDisplay = routeToDisplay)
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


