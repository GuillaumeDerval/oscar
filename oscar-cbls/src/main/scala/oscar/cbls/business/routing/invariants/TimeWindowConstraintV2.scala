package oscar.cbls.business.routing.invariants

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.TTFMatrix
import oscar.cbls.business.routing.invariants.group.{GlobalConstraintDefinition, Segment}
import oscar.cbls.business.routing.model.RoutingConventionMethods
import oscar.cbls.business.routing.model.extensions.TimeWindow
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker

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

object TimeWindowConstraintV2 {
  def apply(routes: ChangingSeqValue,
            v: Int,
            timeWindows: TimeWindow,
            travelTimeMatrix: TTFMatrix,
            violations: Array[CBLSIntVar]): TimeWindowConstraintV2 =
    new TimeWindowConstraintV2(routes: ChangingSeqValue, v, timeWindows, travelTimeMatrix, violations)
}

class TimeWindowConstraintV2 (routes: ChangingSeqValue,
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

  private val transfertFunctionMap: Array[Array[Int => Int]] =
    Array.tabulate(n)(from =>
      Array.tabulate(n)(to =>
        if(from == to) {
          (arrivalTime: Int) => {
            if (arrivalTime < earlylines(from))
              earlylines(from)
            else if(arrivalTime <= deadlines(from))
              arrivalTime
            else
              -1
          }
        }
        else{
          (_) => -1
        }
      ))


  registerStaticAndDynamicDependency(routes)
  finishInitialization()

  for(violation <- violations) violation.setDefiningInvariant(this)

  protected var vehicleSearcher = RoutingConventionMethods.cachedVehicleReachingPosition(routes.value, v)

  var checkpoint : IntSequence = null
  var violationAtCheckpoint:Array[Int] = Array.fill(v)(0)

  // The time window segment at checkpoint
  private val timeWindowSegmentAtCheckPoint: Array[Array[TimeWindowSegment]] =
    Array.tabulate(n)(fromIncluded => {
      Array.tabulate(n)(toIncluded =>
        if(fromIncluded == toIncluded && fromIncluded < v){
          val fts = deadlines(fromIncluded) - earlylines(fromIncluded) - taskDurations(fromIncluded)
          TimeWindowSegment(fromIncluded,toIncluded, earlylines(fromIncluded), earlylines(fromIncluded), 0, fts)
        }
        else {
          val fts = Int.MinValue
          TimeWindowSegment(fromIncluded,toIncluded, earlylines(fromIncluded), earlylines(fromIncluded) + taskDurations(fromIncluded), 0, fts)
        }
      )
    })

  // The amount of time the vehicle waits at a node (before being allowed to start his task)
  private val totalWaitingDurationUntilNodeAtCheckPoint: Array[Int] = Array.fill(n)(0)

  // The leave time at each node
  private val leaveTimeAtNodeAtCheckPoint: Array[Int] = Array.tabulate(n)(node => if(node < v) earlylines(node) else Int.MaxValue)

  private val vehicleChangedSinceCheckpoint:Array[Boolean] = Array.fill(v)(true)
  private var changedVehicleSinceCheckpoint:QList[Int] = vehicles.foldLeft[QList[Int]](null)((acc,v) => QList(v,acc))



  def setVehicleChangedSinceCheckpoint(vehicle:Int) {
    if(!vehicleChangedSinceCheckpoint(vehicle)){
      vehicleChangedSinceCheckpoint(vehicle) = true
      changedVehicleSinceCheckpoint = QList(vehicle,changedVehicleSinceCheckpoint)
    }
  }

  def setAllVehicleChangedSinceCheckpoint(){
    for (vehicle <- vehicles) {
      setVehicleChangedSinceCheckpoint(vehicle)
    }
  }

  def doUpdateAllPrecomputationsToCheckpointAndSaveCheckpoint(): Unit ={
    for (vehicle <- vehicles) {
      violationAtCheckpoint(vehicle) = doUpdatePrecomputationToCheckpointAndSaveCheckpoint(vehicle)
    }
    changedVehicleSinceCheckpoint = null
  }

  def updateAllInvalidPrecomputationsToCheckpointAndSaveCheckpoint(): Unit ={
    while(changedVehicleSinceCheckpoint != null){
      val vehicle = changedVehicleSinceCheckpoint.head
      changedVehicleSinceCheckpoint = changedVehicleSinceCheckpoint.tail
      violationAtCheckpoint(vehicle) = doUpdatePrecomputationToCheckpointAndSaveCheckpoint(vehicle)
    }
  }

  def doUpdatePrecomputationToCheckpointAndSaveCheckpoint(vehicle:Int):Int = ???

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
        if(checkpointLevel == 0){
          this.checkpoint = prev.newValue
          if (!digestUpdates(prev)) {
            doUpdateAllPrecomputationsToCheckpointAndSaveCheckpoint()
            for(vehicle <- vehicles) violations(vehicle) := violationOnVehicleFromScratch(vehicle,checkpoint)
          }else {
            updateAllInvalidPrecomputationsToCheckpointAndSaveCheckpoint()
          }
          vehicleSearcher = RoutingConventionMethods.cachedVehicleReachingPosition(checkpoint, v)
          true
        }else{
          digestUpdates(prev)
        }

      case r@SeqUpdateRollBackToCheckpoint(checkpoint : IntSequence, checkpointLevel:Int) =>
        if(checkpointLevel == 0) {
          require(checkpoint quickEquals this.checkpoint)
          while (changedVehicleSinceCheckpoint != null) {
            val vehicle = changedVehicleSinceCheckpoint.head
            changedVehicleSinceCheckpoint = changedVehicleSinceCheckpoint.tail
            violations(vehicle) := violationAtCheckpoint(vehicle)
            vehicleChangedSinceCheckpoint(vehicle) = false
          }
          true
        }else{
          digestUpdates(r.howToRollBack)
        }

      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>

        if(!digestUpdates(prev)) return false

        println("in insert after digest")
        println("inserting " + value + " at position " + pos)

        val prevNode = prev.newValue.valueAtPosition(pos-1).get
        val nextNode = prev.newValue.valueAtPosition(pos)
        val vehicle = vehicleSearcher(prev.newValue, pos-1)

        setVehicleChangedSinceCheckpoint(vehicle)

        true

      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>

        if(!digestUpdates(prev)) return false

        val prevNode = prev.newValue.valueAtPosition(after).get
        val nextNode = prev.newValue.valueAtPosition(after+1)
        val fromIncludedNode = prev.newValue.valueAtPosition(fromIncluded).get
        val toIncludedNode = prev.newValue.valueAtPosition(toIncluded).get
        val fromVehicle = vehicleSearcher(prev.newValue, fromIncluded)
        val toVehicle = vehicleSearcher(prev.newValue, after)
        println("value : " + prev.newValue.valueAtPosition(fromIncluded).get)
        println("route : " + prev.newValue.toList)

        println("move : " + prevNode, fromIncludedNode, toIncludedNode, nextNode, toVehicle)

        if(fromIncludedNode == toIncludedNode){

          return true
        } else {
          if(flip) return false

        }

        setVehicleChangedSinceCheckpoint(fromVehicle)
        if(fromVehicle != toVehicle)
          setVehicleChangedSinceCheckpoint(toVehicle)

        true

      case x@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        val vehicle = vehicleSearcher(changes.newValue, position)
        setVehicleChangedSinceCheckpoint(vehicle)
        true  // removing a node won't make the route infeasible

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

  override def checkInternals(c : Checker) : Unit = {

    val seq = routes.value
    for(vehicle <- vehicles){
      val actual = violations(vehicle)
      val should = violationOnVehicleFromScratch(vehicle, seq)
      c.check(violations(vehicle).value == violationOnVehicleFromScratch(vehicle,seq),
        Some("violationPerVehicle(vehicle).value=" + violations(vehicle).value
          + " should == violationOnVehicle(vehicle,seq)="+  violationOnVehicleFromScratch(vehicle,seq) + " vehicle:" + vehicle + " route:" + seq))

      val oldUpToDate = vehicleChangedSinceCheckpoint(vehicle)
      vehicleChangedSinceCheckpoint(vehicle) = true
      c.check(violations(vehicle).value == violationOnVehicleFromScratch(vehicle,seq))
      vehicleChangedSinceCheckpoint(vehicle) = oldUpToDate
    }
  }

}

private class TimeWindowConstraintDefinition extends GlobalConstraintDefinition[Int => Int, Boolean] {

  private def transfertFunctionCompositon(from: Int, to: Int, fromFunction: Int => Int, toFunction: Int => Int): Int => Int ={

  }

  /**
    * tis method is called by the framework when a pre-computation must be performed.
    * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
    *
    * @param vehicle      the vehicle where pre-computation must be performed
    * @param routes       the sequence representing the route of all vehicle
    *                     BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    * @param setNodeValue the method that you are expected to use when assigning a value to a node
    *                     BEWARE: you can only apply this method on nodes of the vehicle you are working on
    * @param getNodeValue a method that you can use to get the value associated wit ha node
    *                     BEWARE: you have zero info on when it can generated, so only query the value
    *                     that you have just set through the method setNodeValue.
    *                     also, you should only query the value of node in the route of vehicle "vehicle"
    */
  override def performPreCompute(vehicle: Int, routes: IntSequence, setNodeValue: (Int, Int => Int) => Unit, getNodeValue: Int => Int => Int): Unit = ???

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle   the vehicle that we are focusing on
    * @param segments  the segments that constitute the route.
    *                  The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @param routes    the sequence representing the route of all vehicle
    * @param nodeValue a function that you can use to get the pre-computed value associated with each node (if some has ben given)
    *                  BEWARE: normally, you should never use this function, you only need to iterate through segments
    *                  because it already contains the pre-computed values at the extremity of each segment
    * @return the value associated with the vehicle
    */
  override def computeVehicleValue(vehicle: Int, segments: List[Segment], routes: IntSequence, nodeValue: Int => Option[Int => Int]): Boolean = ???

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    * @param value   the value of the vehicle
    */
  override def assignVehicleValue(vehicle: Int, value: Boolean): Unit = ???

  /**
    * you must also implement this method
    * because the engine needs to know which variable are your output variables
    * so return here all the variables that you will set through the method "assignVehicleValue"
    * typically, this is one variable per vehicle
    *
    * @return all the variables that might be directly modified by the method "assignVehicleValue"
    */
  override def outputVariables: Iterable[Variable] = ???
}