/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by De Landtsheer Renaud and Ghilain Florent.
 * ****************************************************************************
 */

package oscar.cbls.routing.model

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.invariants.lib.set.Cardinality
import oscar.cbls.modeling.Algebra._
import oscar.cbls.search.algo.KSmallest

import scala.collection.immutable.SortedSet
import scala.math.min

/**
 * The class constructor models a VRP problem with N points (deposits and customers)
 * and V vehicles.
 *
 * Vehicles are supposed to leave from their depot, and come back to it.
 * they all have a different depot (but yo ucan put them at the same place if you want)
 *
 * Info: after instantiation, each customer point is unrouted, and each vehicle loop on his deposit.
 * @param N the number of points (deposits and customers) in the problem.
 * @param V the number of vehicles.
 * @param m the model.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
@deprecated("this routing model is inefficient for large moves. A faster model will be released in septembre 2016","spring 2016")
class VRP(val N: Int, val V: Int, val m: Store) {
  /**
   * the data structure array which maintains the successors.
   * It assumed that the V vehicles are indexed from the point 0 to V-1,
   * like that each vehicle is considered like a deposit. Other indexes
   * are used to modelise customers. Finally the value N is used for unrouted node.
   */
  val next: Array[CBLSIntVar] = Array.tabulate(N)(i =>
    if (i < V) CBLSIntVar(m, i, 0 to N - 1, "next" + i)
    else CBLSIntVar(m, N, 0 to N, "next" + i))

  /**unroutes all points of the VRP*/
  def unroute() {
    for (i <- 0 until V) next(i) := i
    for (i <- V until N) next(i) := N
  }

  /**
   * the range of nodes (customers and deposits including) of the problem.
   */
  val nodes = 0 until N
  /**
   * the range vehicle of the problem.
   */
  val vehicles = 0 until V

  /**
   * Returns if a given point is a depot.
   * @param n the point queried.
   * @return true if the point is a depot, else false.
   */
  def isADepot(n: Int): Boolean = { n < V }

  /**
   * Returns if a given point is still routed.
   * @param n the point queried.
   * @return true if the point is still routed, else false.
   */
  def isRouted(n: Int): Boolean = { next(n).newValue != N }

  /**
   * This function is intended to be used for testing only.
   * setCircuit(List(1,2,3,4)) produces the following route :
   * 1 -> 2 -> 3 -> 4 (-> 1)
   */
  def setCircuit(nodes: List[Int]): Unit = {
    def setCircuit(start: Int, nodes: List[Int]): Unit = {
      nodes match {
        case Nil => next(start) := start
        case List(x) => next(x) := start
        case x :: r => next(x) := r.head; setCircuit(start, r)
      }
    }

    nodes match {
      case Nil => ()
      case x :: r => next(x) := r.head; setCircuit(x, r)
    }
  }

  /**
   * @return the list of unrouted nodes as a String.
   */
  def unroutedToString: String = {
    "unrouted: " + nodes.filterNot(isRouted(_)).toList + "\n"
  }

  /**
   * @return the route of a vehicle as a String.
   */
  def routeToString(vehicle: Int): String = {
    "Vehicle " + vehicle + ": " + getRouteOfVehicle(vehicle).mkString("->")
  }

  /**
   * the route of the vehicle, starting at the vehicle node, and not including the last vehicle node
   * @param vehicle
   * @return
   */
  def getRouteOfVehicle(vehicle:Int):List[Int] = {
    var current = next(vehicle).value
    var acc:List[Int] = List(vehicle)
    while (current != vehicle) {
      acc = current :: acc
      current = next(current).newValue //to avoid unnecessary propagation
    }
    acc.reverse
  }

  /**
   * Redefine the toString method.
   * @return the VRP problem as a String.
   */
  override def toString: String = {
    var toReturn = unroutedToString

    for (v <- 0 to V - 1) {
      toReturn += routeToString(v)
      toReturn += "\n"
    }
    for (additionalStringFunction <- additionalStrings) {
      toReturn += additionalStringFunction() + "\n"
    }
    toReturn
  }

  private var additionalStrings: List[() => String] = List.empty
  def addToStringInfo(a: () => String) {
    additionalStrings = a :: additionalStrings
  }
}

/**
 * Maintains the set of routed and unrouted nodes.
 * Info : unrouted nodes are those whose next is N.
 * This trait is abstract, since unrouted can be implemented either stand alone,
 * or as a side effect of other traits
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
abstract trait RoutedAndUnrouted extends VRP {
  /**
   * the data structure set which maintains the routed nodes.
   */
  val routed = Filter(next, _ < N)
  m.registerForPartialPropagation(routed)
  
  val routedNotStartingPoint = routed.minus(SortedSet.empty[Int] ++ (0 to V-1))
  m.registerForPartialPropagation(routedNotStartingPoint)

  /**
   * the data structure set which maintains the unrouted nodes.
   */
  def unrouted: SetValue
}

/**
 * Maintains the set of unrouted nodes.
 * Info : those whose next is N.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
trait UnroutedImpl extends VRP with RoutedAndUnrouted {
  /**
   * the data structure set which maintains the unrouted nodes.
   */
  final override val unrouted = Filter(next, _ == N)
  m.registerForPartialPropagation(unrouted)
}

/**
 * Maintains and fixes a penalty weight of unrouted nodes.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
abstract trait PenaltyForUnrouted extends VRP with RoutedAndUnrouted {
  assert(unrouted != null, "you should put the implementation of Unrouted before PenaltyForUnrouted when declaring your model")

  /**
   * the data structure array which maintains penalty of nodes.
   * it is not supposed to be modified after model close, neither controlled by an invariant
   */
  protected val weightUnroutedPenalty = Array.fill(N)(0)
  /**
   * the variable which maintains the sum of penalty of unrouted nodes, thanks to invariant SumElements.
   */
  val unroutedPenalty = CBLSIntVar(m,name="TotalPenaltyForUnroutedNodes")

  /**
   * It allows you to set the penalty of a given point.
   * @param n the point.
   * @param p the penalty.
   */
  @deprecated("not deprecated, just, do not forget to call closeUnroutedPenaltyWeight afgter you are done with penalties","")
  def setUnroutedPenaltyWeight(n: Int, p: Int) { weightUnroutedPenalty(n) = p }

  /**
   * It allows you to set a specific penalty for all points of the VRP.
   * @param p the penalty.
   */
  def setUnroutedPenaltyWeight(p: Int) { weightUnroutedPenalty.indices.foreach(i => weightUnroutedPenalty(i) = p) }

  def closeUnroutedPenaltyWeight(){
    unroutedPenalty <== Sum(weightUnroutedPenalty, unrouted)
  }

}

/**
 * @author renaud.delandtsheer@cetic.be
 */
trait HopClosestNeighbors extends ClosestNeighbors with HopDistance {
  final override protected def getDistance(from: Int, to: Int): Int = getHop(from, to)
}


abstract trait ClosestNeighborsWithPenaltyForUnrouted extends VRP with PenaltyForUnrouted with ClosestNeighbors{

  var closestNeighborsWithPenaltyForUnrouted: Array[Iterable[Int]] = null

  override def computeClosestNeighbors(): Unit = {
    super.computeClosestNeighbors()
    computeClosestNeighborsWithPenalty()
  }

  private def computeClosestNeighborsWithPenalty() = {
    def arrayOfAllNodes = Array.tabulate(N)(node => node)
    closestNeighborsWithPenaltyForUnrouted = Array.tabulate(N)(node =>
      KSmallest.lazySort(arrayOfAllNodes,
        neighbor => (min(getDistance(neighbor, node), getDistance(node, neighbor)) - weightUnroutedPenalty(neighbor))
      ))
  }

  /**
   * Returns the k nearest nodes of a given node.
   * It allows us to add a filter (optional) on the neighbor.
   *
   * Info : it uses the Currying feature.
   * @param k the parameter k.
   * @param filter the filter, should only return unrouted nodes
   * @param node the given node.
   * @return the k nearest neighbor as an iterable list of Int.
   */
  def kNearestWithPenaltyForUnrouted(k: Int, filter: (Int => Boolean) = (_ => true))(node: Int): Iterable[Int] = {
    if (k >= N - 1) return nodes.filter(filter)

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

    kNearestAccumulator(closestNeighbors(node).iterator, k, Nil)
  }
}

/**
 * Computes the nearest neighbors of each point.
 * Used by some neighborhood searches.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
abstract trait ClosestNeighbors extends VRP {

  protected def getDistance(from: Int, to: Int): Int

  var closestNeighbors: Array[Iterable[Int]] = null

  def computeClosestNeighbors() = {
    def arrayOfAllNodes = Array.tabulate(N)(node => node)
    closestNeighbors = Array.tabulate(N)(node =>
      KSmallest.lazySort(arrayOfAllNodes,
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
   * @param k the parameter k.
   * @param filter the filter.
   * @param node the given node.
   * @return the k nearest neighbor as an iterable list of Int.
   */
  def kNearest(k: Int, filter: (Int => Boolean) = (_ => true))(node: Int): Iterable[Int] = {
    if (k >= N - 1) return nodes.filter(filter)

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

    kNearestAccumulator(closestNeighbors(node).iterator, k, Nil)
  }
}

/**
 * Maintains the hop distance in the VRP, based either on a matrix, or on another mechanism.
 * We consider that a hop distance of Int.MaxVal is unreachable.
 * HopDistance is only handling simple cost functions such as cost matrices
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
trait HopDistance extends VRP {
  /**
   * the data structure which maintains the current hop distance of each node to reach his successor.
   * Info : the domain max is (Int.MaxValue / N) to avoid problem with domain. (allow us to use sum invariant without
   * throw over flow exception to save the distance of all vehicle).
   */
  var hopDistance: Array[IntValue] = new Array[IntValue](N)

  /**
   * maintains the total distance of all vehicle, linked on the actual next hop of each node.
   */
  val overallDistance = CBLSIntVar(m, name = "overall distance")

  def assignOverallDistance() {
    overallDistance <== Sum(hopDistance)
  }
  /**
   * the function which defines the distance between two points of the VRP.
   */
  var distanceFunction: ((Int, Int) => Int) = null

  /**
   * This method sets the function distance with a distance matrix.
   * If a more complex function is to be used, set a controlling invariant to the hopDistances yourself.
   * It considers distance from a node to itself as zero.
   * @param DistanceMatrix the distance between each point.
   */
  def installCostMatrix(DistanceMatrix: Array[Array[Int]]) {
    distanceFunction = (i: Int, j: Int) => DistanceMatrix(i)(j)
    for (i <- 0 until N) hopDistance(i) = new Int2Int(next(i), j => { if (j != N) DistanceMatrix(i)(j) else 0 })
    assignOverallDistance()
  }

  /**
   * This method sets the distance to use for the hop between points thanks
   * to a given function.
   * @param fun the function which defines the distance between two points.
   */
  def installCostFunction(fun: (Int, Int) => Int) {
    distanceFunction = fun
    for (i <- 0 until N) hopDistance(i) = new Int2Int(next(i), j => fun(i, j))
    assignOverallDistance()
  }

  def installhopDistance(d: Domain = 0 to Int.MaxValue / N) {
    hopDistance = Array.tabulate(N) { (i: Int) => CBLSIntVar(m, 0, 0 to Int.MaxValue / N, "hopDistanceForLeaving" + i) }
    assignOverallDistance()
  }
  /**
   * Returns the distance from a given node (start node) to another given node (end node) of the VRP.
   * @param from the start node
   * @param to the end node
   * @return the distance between the start and end node as an Int.
   */
  def getHop(from: Int, to: Int): Int = distanceFunction(from, to)
}


trait hopDistancePerVehicle extends HopDistance with NodesOfVehicle{
  var hopDistancePerVehicle:Array[IntValue] = null
  def installHopDistancePerVehicle(){
    hopDistancePerVehicle = Array.tabulate(V)(v => Sum(hopDistance,nodesOfVehicle(v)).setName("totalDistance_" + v))
    addToStringInfo(() => "hopDistancePerVehicle:" + hopDistancePerVehicle.mkString(";"))
  }
}

trait hopsPerVehicle extends NodesOfVehicle{
  var hopsPerVehicle:Array[IntValue] = Array.tabulate(V)(v => Cardinality(nodesOfVehicle(v)).setName("totalHops_" + v))
  addToStringInfo(() => "hopsPerVehicle:" + hopsPerVehicle.mkString(";"))
}

/**
 * Maintains the set of nodes reached by each vehicle
 * @author renaud.delandtsheer@cetic.be
 */
trait NodesOfVehicle extends PositionInRouteAndRouteNr with RoutedAndUnrouted {
  val nodesOfVehicle = Cluster.MakeDense(routeNr).clusters
  final override val unrouted = nodesOfVehicle(V)
}

/**
 * Maintains the position of nodes in the routes, the route number of each node,
 * the length of each route and their last node.
 * that these output variables are registered for a grouped partial propagation
 * to ensure some efficiency in the queries proposed by this trait.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
trait PositionInRouteAndRouteNr extends VRP {
  /**
   * the invariant Routes.
   */
  val routes = Routes.buildRoutes(next, V)

  /**
   * the position in route of each node as an array of IntVar.
   */
  val positionInRoute = routes.positionInRoute

  /**
   * the route number of each node as an array of IntVar.
   */
  val routeNr = routes.routeNr

  /**
   * the route length of each route as an array of IntVar.
   */
  val routeLength = routes.routeLength

  {
    val allvars = positionInRoute.toList ++ routeNr ++ routeLength
    m.registerForPartialPropagation(allvars: _*)
  }

  /**
   * Tells if twos given nodes form a segment of route of n minimum length.
   * @param fromNode the start of potential segment.
   * @param toNode the end of potential segment.
   * @param n the minimum length of segment.
   * @return true if "fromNode" to "toNode" forms a segment of route of n minimum length, else false.
   */
  def isAtLeastAsFarAs(fromNode: Int, toNode: Int, n: Int): Boolean = {
    routeNr(fromNode).value == routeNr(toNode).value &&
      positionInRoute(fromNode).value + n <= positionInRoute(toNode).value
  }

  /**
   * Tells if two given nodes form a segment of route of n maximum length.
   * @param fromNode the start of potential segment.
   * @param toNode the end of potential segment.
   * @param n the maximum length of route.
   * @return true if "fromNode" to "toNode" forms a segment of route of n maximum length, else false.
   */
  def isAtMostAsFarAs(fromNode: Int, toNode: Int, n: Int): Boolean = {
    routeNr(fromNode).value == routeNr(toNode).value &&
      positionInRoute(fromNode).value + n >= positionInRoute(toNode).value
  }

  /**
   * Tells if two given nodes form a segment of route.
   * @param fromNode the start of potential segment.
   * @param toNode the end of potential segment.
   * @return true if "fromNode" to "toNode" form a segment of route, else false.
   */
  def isASegment(fromNode: Int, toNode: Int): Boolean = {
    isAtLeastAsFarAs(fromNode, toNode, 1)
  }

  /**
   * Tells if a given node is in a segment of route between fromNode and toNode.
   * @param node the given node queried.
   * @param fromNode the start of the segment of route.
   * @param toNode the end of the segment of route.
   * @return true if node is in a segment of route between "fromNode" and "toNode", else false.
   */
  def isBetween(node: Int, fromNode: Int, toNode: Int): Boolean = {
    if (isASegment(fromNode, toNode)) {
      routeNr(fromNode).value == routeNr(node).value &&
        positionInRoute(fromNode).value <= positionInRoute(node).value &&
        positionInRoute(node).value < positionInRoute(toNode).value
    } else false
  }

  /**
   * Tells if two given nodes are on the same route.
   * ( i.e. they have the same route number)
   * @param n the first given node.
   * @param m the second given node.
   */
  def onTheSameRoute(n: Int, m: Int): Boolean = {
    routeNr(n).value == routeNr(m).value
  }
}

/**
 * Maintains a penalty weight for routes which do not contain task nodes.
 * That is: they only contain the vehicle node.
 * @author yoann.guyot@cetic.be
 */
trait PenaltyForEmptyRoute extends VRP with PositionInRouteAndRouteNr {
  /**
   * The data structure array which maintains route penalty.
   */
  private val emptyRoutePenaltyWeight: Array[CBLSIntVar] =
    Array.tabulate(V)(v =>
      CBLSIntVar(m, 0, FullRange, "penality of vehicule " + v))

  /**
   * The variable which maintains the set of empty routes.
   * (that is: routes containing no other node than the vehicle node)
   */
  val emptyRoutes = Filter(routeLength, _ <= (1))

  /**
   * The variable which maintains the sum of route penalties,
   * thanks to SumElements invariant.
   */
  val emptyRoutePenalty = Sum(emptyRoutePenaltyWeight, emptyRoutes)

  /**
   * Allows client to set the penalty of a given vehicle route.
   * @param n the node.
   * @param p the penalty.
   */
  def setEmptyRoutePenaltyWeight(n: Int, p: Int) {
    emptyRoutePenaltyWeight(n) := p
  }

  /**
   * Allows client to set a specific penalty for all the VRP routes.
   * @param p the penalty.
   */
  def setEmptyRoutePenaltyWeight(p: Int) {
    emptyRoutePenaltyWeight.foreach(penalty => penalty := p)
  }
}

trait PenaltyForEmptyRouteWithException extends VRP with NodesOfVehicle {
  /**
   * The data structure array which maintains route penalty.
   */
  private val emptyRoutePenaltyWeight: Array[CBLSIntVar] =
    Array.tabulate(V)(v =>
      CBLSIntVar(m, name = "penality of vehicule " + v))

  val exceptionNodes: CBLSSetVar = new CBLSSetVar(m, SortedSet.empty, 0 to N - 1, "NodesNotToConsiderForEmptyRoutes")

  private val nodesOfRealVehicles = Array.tabulate(V)(nodesOfVehicle)

  /**
   * The variable which maintains the set of empty routes.
   * (that is: routes containing no other node than the vehicle node)
   */
  val emptyRoutes = Filter(nodesOfRealVehicles.map(
    (vehicleNodes: CBLSSetVar) => Cardinality(vehicleNodes minus exceptionNodes)), _ == 1)

  /**
   * The variable which maintains the sum of route penalties,
   * thanks to SumElements invariant.
   */
  val emptyRoutePenalty = Sum(emptyRoutePenaltyWeight, emptyRoutes)

  /**
   * Allows client to set the penalty of a given vehicle route.
   * @param n the node.
   * @param p the penalty.
   */
  def setEmptyRoutePenaltyWeight(n: Int, p: Int) {
    emptyRoutePenaltyWeight(n) := p
  }

  /**
   * Allows client to set a specific penalty for all the VRP routes.
   * @param p the penalty.
   */
  def setEmptyRoutePenaltyWeight(p: Int) {
    emptyRoutePenaltyWeight.foreach(penalty => penalty := p)
  }
}

/**
 * This trait maintains the predecessors of each node of the VRP.
 * It uses the Predecessor invariant.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
trait Predecessors extends VRP {
  /**
   * the data structure array which maintains the predecessors of each node.
   */
  val preds: Array[IntValue] = Predecessor(next, V).preds
}

