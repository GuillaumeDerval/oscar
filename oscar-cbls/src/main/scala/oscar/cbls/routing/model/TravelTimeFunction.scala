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
 *     This code has been initially developed by Renaud De Landtsheer
 * ****************************************************************************
 */

package oscar.cbls.routing.model

import oscar.cbls.constraints.lib.basic.GE
import oscar.cbls.constraints.lib.basic.LE
import oscar.cbls.invariants.core.computation.CBLSIntConst
import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.invariants.core.computation.Domain.rangeToDomain
import oscar.cbls.invariants.core.computation.IntValue
import oscar.cbls.invariants.core.computation.IntValue.int2IntValue
import oscar.cbls.invariants.lib.logic.IntITE
import oscar.cbls.invariants.lib.logic.IntInt2Int
import oscar.cbls.invariants.lib.minmax.Max2
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.modeling.Algebra.InstrumentArrayOfIntValue
import oscar.cbls.modeling.Algebra.InstrumentInt
import oscar.cbls.modeling.Algebra.InstrumentIntVar

/**
 * an abstract class representing a travel time function
 * @author renaud.delandtsheer@cetic.be
 */
abstract class TravelTimeFunction {
  def getTravelDuration(from: Int, leaveTime: Int, to: Int): Int
  def getBackwardTravelDuration(from: Int, arrivalTime: Int, to: Int): Int

  def getMinMaxTravelDuration(from: Int, to: Int): (Int, Int) =
    (getMinTravelDuration(from, to), getMaxTravelDuration(from, to))

  def getMinTravelDuration(from: Int, to: Int): Int
  def getMaxTravelDuration(from: Int, to: Int): Int
}

/**
 * adds the notion of time to your VRP
 * @author renaud.delandtsheer@cetic.be
 */
trait Time extends VRP with Predecessors {
  val defaultArrivalTime = new CBLSIntConst(0)
  //TODO: on peut améliorer le codate en enlevant des variables.
  val arrivalTime = Array.tabulate(N) {
    (i: Int) => CBLSIntVar(m, 0, 0 to Int.MaxValue / N, "arrivalTimeAtNode" + i)
  }
  val leaveTime = Array.tabulate(N) {
    (i: Int) => CBLSIntVar(m, 0, 0 to Int.MaxValue / N, "leaveTimeAtNode" + i)
  }
  val travelOutDuration = Array.tabulate(N) {
    (i: Int) => CBLSIntVar(m, 0, 0 to Int.MaxValue / N, "travelDurationToLeave" + i)
  }
  val arrivalTimeToNext = Array.tabulate(N + 1) {
    (i: Int) =>
      if (i == N) defaultArrivalTime
      else (travelOutDuration(i) + leaveTime(i))
  }

  def setNodeDuration(node: Int, duration: IntValue) {
    assert(node >= V)
    leaveTime(node) <== arrivalTime(node) + duration
  }

  for (i <- 0 to N - 1) {
    arrivalTime(i) <== arrivalTimeToNext.element(preds(i))
  }

  addToStringInfo(() => "arrivalTime:      " + arrivalTime.toList.mkString(","))
  addToStringInfo(() => "leaveTime:        " + leaveTime.toList.mkString(","))
  addToStringInfo(() => "travelOutDuration:" + travelOutDuration.toList.mkString(","))
  addToStringInfo(() => "arrivalTimeToNext:" + arrivalTimeToNext.toList.mkString(","))
}

/**
 * when the cost of a hop is more complex than a distance matrix.
 * Beware, you must still define the leaveTime from the ArrivalTime (or not)
 * and you can post strong constraints on these values
 * @author renaud.delandtsheer@cetic.be
 */
trait TravelTimeAsFunction extends VRP with Time {

  var travelCosts: TravelTimeFunction = null

  /**
   * sets the cost function
   * @param travelCosts
   */
  def setTravelTimeFunctions(travelCosts: TravelTimeFunction) {
    this.travelCosts = travelCosts
    for (i <- 0 to N - 1) {
      travelOutDuration(i) <== new IntInt2Int(leaveTime(i), next(i),
        (leaveTime, successor) =>
          if (successor == N) 0
          else travelCosts.getTravelDuration(i, leaveTime, successor))
    }
  }
}

/**
 * to post time window constraints
 * @author renaud.delandtsheer@cetic.be
 */
trait TimeWindow extends Time with StrongConstraints {

  def setEndWindow(node: Int, endWindow: Int) {
    require(node >= V, "only for specifying time windows on nodes, not on vehicles")
    strongConstraints.post(LE(IntITE(next(node), 0, leaveTime(node), N - 1), endWindow).nameConstraint("end of time window on node " + node))
  }

  def setVehicleEnd(vehicle: Int, endTime: Int) {
    require(vehicle < V, "only for specifying end time of vehicles")
    strongConstraints.post(LE(arrivalTime(vehicle), endTime).nameConstraint("end of time for vehicle " + vehicle))
  }

  def setNodeDuration(node: Int, durationWithoutWait: IntValue, startWindow: Int) {
    leaveTime(node) <== Max2(arrivalTime(node), startWindow) + durationWithoutWait
  }

  def setNodeDuration(node: Int, durationWithoutWait: IntValue, startWindow: Int, maxWaiting: Int) {
    setNodeDuration(node, durationWithoutWait, startWindow)
    strongConstraints.post(GE(arrivalTime(node), startWindow - maxWaiting).nameConstraint("end of time window on node (with duration)" + node))
  }

}

/**
 * addition ot the [[oscar.cbls.routing.model.TimeWindow]] trait, adds a variable representing the waiting duration
 * @author renaud.delandtsheer@cetic.be
 */
trait WaitingDuration extends TimeWindow {
  val waitingDuration = Array.tabulate(N) {
    (i: Int) => CBLSIntVar(m, 0, 0 to Int.MaxValue / N, "WaitingDurationBefore" + i)
  }

  def setNodeDurationAndWaitingTime(node: Int, durationWithoutWait: IntValue, waitingDuration: IntValue) {
    super.setNodeDuration(node, durationWithoutWait + waitingDuration)
    this.waitingDuration(node) <== waitingDuration
  }

  override def setNodeDuration(node: Int, durationWithoutWait: IntValue, startWindow: Int) {
    super.setNodeDuration(node, durationWithoutWait, startWindow)
    waitingDuration(node) <== Max2(0, startWindow - arrivalTime(node))
  }

  override def setNodeDuration(node: Int, durationWithoutWait: IntValue, startWindow: Int, maxWaiting: Int) {
    setNodeDuration(node, durationWithoutWait, startWindow)
    strongConstraints.post(LE(waitingDuration(node), maxWaiting).nameConstraint("max waiting duration before node " + node))
  }
}

/**
 * Computes the nearest neighbors of each point.
 * Used by some neighborhood searches.
 * @author renaud.delandtsheer@cetic.be
 */
trait TimeClosestNeighbors extends ClosestNeighbors with TravelTimeAsFunction {
  final override protected def getDistance(from: Int, to: Int): Int = {
    travelCosts.getMinTravelDuration(from, to)
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
trait TotalTimeSpentByVehiclesOutOfDepotAsObjectiveTerm extends VRPObjective with Time {
  for (v <- 0 to V - 1) {
    addObjectiveTerm(arrivalTime(v) - leaveTime(v))
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
trait TimeSpentOnRouteAsObjectiveTerm extends VRPObjective with Time {
  addObjectiveTerm(Sum(travelOutDuration))
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
trait WaitingTimeAsObjectiveTerm extends VRPObjective with WaitingDuration {
  addObjectiveTerm(Sum(waitingDuration))
}

