package oscar.cbls.routing.seq.model

import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.constraints.lib.basic.{GE, LE}
import oscar.cbls.invariants.core.computation.{IntValue, CBLSIntVar, CBLSIntConst}
import oscar.cbls.invariants.lib.logic.{IntITE, IntInt2Int}
import oscar.cbls.invariants.lib.minmax.Max2
import oscar.cbls.modeling.Algebra._

/**
  * an abstract class representing a travel time function
  *
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
  *
  * @author renaud.delandtsheer@cetic.be
  */
trait Time extends VRP {
  val defaultArrivalTime = new CBLSIntConst(0)
  //TODO: on peut améliorer le codate en enlevant des variables.
  val arrivalTime = Array.tabulate(n) {
    (i: Int) => CBLSIntVar(m, 0, 0 to Int.MaxValue / n, "arrivalTimeAtNode" + i)
  }
  val leaveTime = Array.tabulate(n) {
    (i: Int) => CBLSIntVar(m, 0, 0 to Int.MaxValue / n, "leaveTimeAtNode" + i)
  }
  val travelOutDuration = Array.tabulate(n) {
    (i: Int) => CBLSIntVar(m, 0, 0 to Int.MaxValue / n, "travelDurationToLeave" + i)
  }
  val arrivalTimeToNext = Array.tabulate(n + 1) {
    (i: Int) =>
      if (i == n) defaultArrivalTime
      else travelOutDuration(i) + leaveTime(i)
  }

  def setNodeDuration(node: Int, duration: IntValue) {
    assert(node >= v)
    leaveTime(node) <== arrivalTime(node) + duration
  }

  for (i <- 0 until n) {
    arrivalTime(i) <== arrivalTimeToNext.element(prev(i))
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
  *
  * @author renaud.delandtsheer@cetic.be
  */
trait TravelTimeAsFunction extends Time {

  var travelDurationMatrix: TravelTimeFunction = null

  /**
    * sets the cost function
    *
    * @param travelCosts
    */
  def setTravelTimeFunctions(travelCosts: TravelTimeFunction) {
    this.travelDurationMatrix = travelCosts
    for (i <- 0 to n - 1) {
      travelOutDuration(i) <== new IntInt2Int(leaveTime(i), next(i),
        (leaveTime, successor) =>
          if (successor == n) 0
          else travelCosts.getTravelDuration(i, leaveTime, successor))
    }
  }
}

/**
  * to post time window constraints
  *
  * @author renaud.delandtsheer@cetic.be
  */
trait TimeWindow extends Time {

  def setEndWindow(node: Int, endWindow: Int, constraintSystem: ConstraintSystem) {
    require(node >= v, "only for specifying time windows on nodes, not on vehicles")
    constraintSystem.post(LE(IntITE(next(node), 0, leaveTime(node), n - 1), endWindow).nameConstraint("end of time window on node " + node))
  }

  def setVehicleEnd(vehicle: Int, endTime: Int, constraintSystem: ConstraintSystem) {
    require(vehicle < v, "only for specifying end time of vehicles")
    constraintSystem.post(LE(arrivalTime(vehicle), endTime).nameConstraint("end of time for vehicle " + vehicle))
  }

  def setNodeDuration(node: Int, durationWithoutWait: IntValue, startWindow: Int) {
    leaveTime(node) <== Max2(arrivalTime(node), startWindow) + durationWithoutWait
  }

  def setNodeDuration(node: Int, durationWithoutWait: IntValue, startWindow: Int, maxWaiting: Int, constraintSystem: ConstraintSystem) {
    setNodeDuration(node, durationWithoutWait, startWindow)
    constraintSystem.post(GE(arrivalTime(node), startWindow - maxWaiting).nameConstraint("end of time window on node (with duration)" + node))
  }

}

/**
  * addition ot the [[oscar.cbls.routing.model.TimeWindow]] trait, adds a variable representing the waiting duration
  *
  * @author renaud.delandtsheer@cetic.be
  */
trait WaitingDuration extends TimeWindow {
  val waitingDuration = Array.tabulate(n) {
    (i: Int) => CBLSIntVar(m, 0, 0 to Int.MaxValue / n, "WaitingDurationBefore" + i)
  }

  def setNodeDurationAndWaitingTime(node: Int, durationWithoutWait: IntValue, waitingDuration: IntValue) {
    super.setNodeDuration(node, durationWithoutWait + waitingDuration)
    this.waitingDuration(node) <== waitingDuration
  }

  override def setNodeDuration(node: Int, durationWithoutWait: IntValue, startWindow: Int) {
    super.setNodeDuration(node, durationWithoutWait, startWindow)
    waitingDuration(node) <== Max2(0, startWindow - arrivalTime(node))
  }

  override def setNodeDuration(node: Int, durationWithoutWait: IntValue, startWindow: Int, maxWaiting: Int, constraintSystem: ConstraintSystem) {
    setNodeDuration(node, durationWithoutWait, startWindow)
    constraintSystem.post(LE(waitingDuration(node), maxWaiting).nameConstraint("max waiting duration before node " + node))
  }
}

/**
  * Computes the nearest neighbors of each point.
  * Used by some neighborhood searches.
  *
  * @author renaud.delandtsheer@cetic.be
  */
trait TimeClosestNeighbors extends ClosestNeighbors with TravelTimeAsFunction {
  final override protected def getDistance(from: Int, to: Int): Int = {
    travelDurationMatrix.getMinTravelDuration(from, to)
  }
}

/*
/**
  * @author renaud.delandtsheer@cetic.be
  */
trait TotalTimeSpentByVehiclesOutOfDepotAsObjectiveTerm extends Time {
  for (v <- 0 to v - 1) {
    addObjectiveTerm(arrivalTime(v) - leaveTime(v))
  }
}

/**
  * @author renaud.delandtsheer@cetic.be
  */
trait TimeSpentOnRouteAsObjectiveTerm extends Time {
  addObjectiveTerm(Sum(travelOutDuration))
}

/**
  * @author renaud.delandtsheer@cetic.be
  */
trait WaitingTimeAsObjectiveTerm extends WaitingDuration {
  addObjectiveTerm(Sum(waitingDuration))
}*/