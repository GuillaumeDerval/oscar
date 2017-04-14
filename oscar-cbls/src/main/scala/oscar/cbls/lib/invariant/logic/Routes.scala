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
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer and Florent Ghilain.
 * ****************************************************************************
 */

package oscar.cbls.lib.invariant.logic

import oscar.cbls.algo.heap.BinomialHeap
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker

/**
 * This invariants maintains data structures representing a VRP and his
 * characteristics like the length of route, the position of points in route, etc.. .
 *
 * Info : the indices from 0 to V-1 (in the next, positionInRoute and routeNr array) are the starting
 * points of vehicles.
 * @param V the number of vehicles.
 * @param externalNext the array of successors of each points (deposits and customers) of the VRP.
 * @param positionInRoute the position in route of each points, N is the value of unrouted node.
 * @param routeNr the route number of each points, V is the value of unrouted node.
 * @param routeLength the length of each route.
 * @param lastInRoute the last point in each route.
 * @author renaud.delandtsheer@cetic.be
 * */
class Routes(V: Int,
  val externalNext: Array[IntValue],
  val positionInRoute: Array[CBLSIntVar],
  val routeNr: Array[CBLSIntVar],
  val routeLength: Array[CBLSIntVar],
  val lastInRoute: Array[CBLSIntVar])
  extends Invariant
  with IntNotificationTarget{

  val UNROUTED = externalNext.length

  registerStaticAndDynamicDependencyArrayIndex(externalNext)
  finishInitialization()
  for (v <- positionInRoute) { v.setDefiningInvariant(this) }
  for (v <- routeNr) { v.setDefiningInvariant(this) }
  for (v <- routeLength) { v.setDefiningInvariant(this) }
  for (v <- lastInRoute) { v.setDefiningInvariant(this) }

  var next:Array[Int] = externalNext.map(_.value)

  for (v <- 0 until V) DecorateVehicleRoute(v)

  override def toString: String = {
    var toReturn: String = ""
    toReturn += "\nNext array: ["
    for (v <- next) { toReturn += ("" + v + ",") }
    toReturn = toReturn.substring(0, toReturn.length - 1) + "]\n"
    toReturn += "Position array: ["
    for (v <- positionInRoute) { toReturn += ("" + v.newValue + ",") }
    toReturn = toReturn.substring(0, toReturn.length - 1) + "]\n"
    toReturn += "RouteNr array: ["
    for (v <- routeNr) { toReturn += ("" + v.newValue + ",") }
    toReturn = toReturn.substring(0, toReturn.length - 1) + "]\n"
    toReturn += "RouteLength array: ["
    for (v <- routeLength) { toReturn += ("" + v.newValue + ",") }
    toReturn = toReturn.substring(0, toReturn.length - 1) + "]\n"
    toReturn += "LastInRoute array: ["
    for (v <- lastInRoute) { toReturn += ("" + v.newValue + ",") }
    toReturn = toReturn.substring(0, toReturn.length - 1) + "]\n"
    toReturn
  }

  def DecorateVehicleRoute(v: Int) {
    var currentID = v
    var currentPosition = 1
    positionInRoute(v) := 0
    routeNr(v) := v
    while (next(currentID) != v) {

      assert(next(currentID) > v)

      currentID = next(currentID)
      positionInRoute(currentID) := currentPosition
      routeNr(currentID) := v
      currentPosition += 1
    }
    lastInRoute(v) := currentID
    routeLength(v) := positionInRoute(currentID).newValue + 1
  }

  var toUpdateList: List[Int] = List.empty
  var toUpdateCount: Int = 0
  val isInToUpdateArray:Array[Boolean] = Array.fill(UNROUTED)(false)

  override def notifyIntChanged(v: ChangingIntValue, i: Int, OldVal: Int, NewVal: Int) {
    if(!isInToUpdateArray(i)) {
      toUpdateList = i :: toUpdateList
      toUpdateCount += 1
      isInToUpdateArray(i) = true
    }
    scheduleForPropagation()
    assert(next(i) == OldVal)
    next(i) = NewVal
  }

  @inline
  final def isUpToDate(node: Int): Boolean = {
    ((routeNr(node).newValue == routeNr(next(node)).newValue)
      && ((positionInRoute(node).newValue + 1) % UNROUTED == positionInRoute(next(node)).newValue))
  }

  //TODO how about an accumulating heap?
  val heap = new BinomialHeap[(Int, Int)]((a: (Int, Int)) => a._2, UNROUTED)

  override def performInvariantPropagation() {
    for (node <- toUpdateList) {
      isInToUpdateArray(node) = false
      if (next(node) == UNROUTED) {
        //node is unrouted now
        routeNr(node) := V
        positionInRoute(node) := UNROUTED
      } else if (isUpToDate(node)) {
        ;
      } else {
        heap.insert((node, positionInRoute(node).newValue))
      }
    }
    toUpdateList = List.empty
    toUpdateCount = 0

    while (!heap.isEmpty) {
      val currentNodeForUpdate = heap.popFirst()._1
      DecorateRouteStartingFromAndUntilConformOrEnd(currentNodeForUpdate)
    }
  }

  /**
   * @param nodeID is the node whose next has changed
   */
  def DecorateRouteStartingFromAndUntilConformOrEnd(nodeID: Int) {
    var currentNode:Int = nodeID
    var nextNode:Int = next(currentNode)
    var maxIt:Int = UNROUTED
    while (!isUpToDate(currentNode) && nextNode >= V) {
      positionInRoute(nextNode) := (positionInRoute(currentNode).newValue + 1)
      routeNr(nextNode) := routeNr(currentNode).newValue
      currentNode = nextNode
      nextNode = next(currentNode)
      if (maxIt == 0) throw new Error("Route invariant not converging. Cycle involving node " + currentNode)
      maxIt -= 1
    }
    if (nextNode < V) {
      lastInRoute(nextNode) := currentNode
      routeLength(nextNode) := positionInRoute(currentNode).newValue + 1
    }
  }

  override def checkInternals(c: Checker) {
    for (n <- next.indices) {
      val nextNode = next(n)
      if (nextNode != UNROUTED) {
        c.check(routeNr(nextNode).value == routeNr(n).value, Some("routeNr(nextNode).value == routeNr(n).value"))
        if (nextNode < V) {
          c.check(positionInRoute(nextNode).value == 0, Some("positionInRoute(nextNode).value == 0"))
          c.check(routeNr(nextNode).value == nextNode, Some("routeNr(nextNode).value == nextNode"))
        } else {
          c.check(positionInRoute(nextNode).value == (positionInRoute(n).value + 1) % (routeLength(routeNr(n).value).value),
            Some("positionInRoute(nextNode).value == (positionInRoute(n).value +1)%(routeLength(routeNr(n).value).value)"))
          c.check(routeNr(n).value == routeNr(nextNode).value, Some("routeNr(n).value == routeNr(nextNode).value"))
        }
      } else {
        c.check(routeNr(n).value == V, Some("routeNr(n).value == V"))
        c.check(positionInRoute(n).value == UNROUTED, Some("positionInRoute(n).value == UNROUTED"))
      }
    }
  }
}


object Routes {
  def buildRoutes(next: Array[IntValue], V: Int) = {
    val m: Store = InvariantHelper.findModel(next)

    val positionInRoute = Array.tabulate(next.length)(i => CBLSIntVar(m, next.length, 0 to next.length, "PositionInRouteOfPt" + i))
    val routeNr = Array.tabulate(next.length)(i => CBLSIntVar(m, V, 0 to V, "RouteNrOfPt" + i))
    val routeLength = Array.tabulate(V)(i => CBLSIntVar(m, 0, 0 to next.length, "Route " + i + "-Lenght"))
    val lastInRoute = Array.tabulate(V)(i => CBLSIntVar(m, i, 0 to next.length, "LastInRoute " + i))

    new Routes(V, next, positionInRoute, routeNr, routeLength, lastInRoute)
  }
}
