package oscar.cbls.business.routing.invariants

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.TTFMatrix
import oscar.cbls.business.routing.model.RoutingConventionMethods
import oscar.cbls.business.routing.model.extensions.TimeWindow
import oscar.cbls.core.computation._

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

object TimeWindowConstraint {
  def apply(routes: ChangingSeqValue,
            v: Int,
            timeWindows: TimeWindow,
            travelTimeMatrix: TTFMatrix,
            violations: Array[CBLSIntVar]): TimeWindowConstraint =
    new TimeWindowConstraint(routes: ChangingSeqValue, v, timeWindows, travelTimeMatrix, violations)
}

class TimeWindowConstraint (routes: ChangingSeqValue,
                            v: Int,
                            timeWindows: TimeWindow,
                            travelTimeMatrix: TTFMatrix,
                            violations: Array[CBLSIntVar]
                           ) extends Invariant with SeqNotificationTarget {

  val n: Int = routes.maxValue + 1
  val vehicles: Range = 0 until v

  private val earlylines: Array[Int] = timeWindows.earlylines
  private val deadlines: Array[Int] = timeWindows.deadlines
  private val taskDurations: Array[Int] = timeWindows.taskDurations

  registerStaticAndDynamicDependency(routes)
  finishInitialization()

  for(violation <- violations) violation.setDefiningInvariant(this)

  protected var vehicleSearcher = RoutingConventionMethods.cachedVehicleReachingPosition(routes.value, v)

  var checkpoint : IntSequence = null

  // How many unit of time we can move forward the leave time without violating the time window.
  private val forwardTimeSlackAtCheckPoint: Array[Int] = Array.tabulate(n)(
    node => deadlines(node) - earlylines(node) - taskDurations(node))

  // How many unit of time we can move backward the leave time without violating the time window AND
  // without adding waiting time afterward.
  private val backwardTimeSlackAtCheckPoint: Array[Int] = Array.fill(n)(0)

  // The amount of time the vehicle waits at a node (before being allowed to start his task)
  private val waitingDurationAtNodeAtCheckPoint: Array[Int] = Array.fill(n)(0)

  // The arrival time at each node
  private val arrivalTimeAtNodeAtCheckPoint: Array[Int] = Array.fill(n)(Int.MaxValue)

  // The leave time at each node
  private val leaveTimeAtNodeAtCheckPoint: Array[Int] = Array.fill(n)(Int.MaxValue)

  private val vehicleChangedSinceCheckpoint:Array[Boolean] = Array.fill(v)(true)
  private var changedVehicleSinceCheckpoint:QList[Int] = vehicles.foldLeft[QList[Int]](null)((acc,v) => QList(v,acc))

  private def isForbidden(node: Int): Boolean ={
    (Math.min(0, forwardTimeSlackAtCheckPoint(node)) +
      Math.min(0, backwardTimeSlackAtCheckPoint(node))) > 0
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    if (!digestUpdates(changes)){
      for (vehicle <- vehicles) {
        violations(vehicle) := violationOnVehicleFromScratch(vehicle, changes.newValue)
      }
    }

  }

  private def digestUpdates(changes: SeqUpdate): Boolean ={
    changes match {
      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isStarMode : Boolean, checkpointLevel:Int) =>
        false

      case r@SeqUpdateRollBackToCheckpoint(checkpoint : IntSequence, checkpointLevel:Int) =>
        false

      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        false

      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        false

      case x@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        false

      case SeqUpdateLastNotified(value : IntSequence) =>
        true //we are starting from the previous value

      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
    }
  }

  private def violationOnVehicleFromScratch(vehicle: Int, seq: IntSequence): Int ={
    var arrivalTimeAtFromNode = earlylines(vehicle)
    var leaveTimeAtFromNode = earlylines(vehicle)
    var fromNode = vehicle
    val explorerAtVehicleStart = seq.explorerAtAnyOccurrence(vehicle).head
    var explorerAtCurrentNode = explorerAtVehicleStart.next
    var violationFound = false

    while(explorerAtCurrentNode.isDefined && explorerAtCurrentNode.get.value >= v && !violationFound){
      val toNode = explorerAtCurrentNode.get.value
      val travelDuration = travelTimeMatrix.getTravelDuration(fromNode, leaveTimeAtFromNode, toNode)
      val arrivalTimeAtToNode = leaveTimeAtFromNode + travelDuration
      val leaveTimeAtToNode = Math.max(earlylines(toNode), arrivalTimeAtToNode) + taskDurations(toNode)

      // Check violation
      if(leaveTimeAtToNode > deadlines(toNode))
        violationFound = true

      // Update values
      fromNode = toNode
      explorerAtCurrentNode = explorerAtCurrentNode.get.next
      arrivalTimeAtFromNode = arrivalTimeAtToNode
      leaveTimeAtFromNode = leaveTimeAtToNode
    }

    // Check travel back to depot
    val travelBackToDepot = travelTimeMatrix.getTravelDuration(fromNode, leaveTimeAtFromNode, vehicle)
    val arrivalTimeAtDepot = leaveTimeAtFromNode + travelBackToDepot
    if(violationFound || arrivalTimeAtDepot >= deadlines(vehicle)) 1 else 0
  }


}
