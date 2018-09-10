package oscar.cbls.business.routing.invariants

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.TTFMatrix
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

  private def concatForwardTimeSlack(firstSegment: TimeWindowSegment, secondSegment: TimeWindowSegment): TimeWindowSegment ={
    val travelFromFirstSegmentToSecondSegment =
      travelTimeMatrix.getTravelDuration(
        firstSegment.toIncluded,
        firstSegment.toIncludedLeavingTime,
        secondSegment.fromIncluded)

    val concatenationFTS = Math.min(
      firstSegment.forwardTimeSlack,
      secondSegment.forwardTimeSlack + firstSegment.totalWaitingDuration +
        secondSegment.fromIncludedLeavingTime -
        (firstSegment.toIncludedLeavingTime + travelFromFirstSegmentToSecondSegment + taskDurations(secondSegment.fromIncluded)))

    val delta = firstSegment.toIncludedLeavingTime + travelFromFirstSegmentToSecondSegment - secondSegment.fromIncludedLeavingTime

    val concatenationLeavingTime = secondSegment.toIncludedLeavingTime + Math.max(0, delta - secondSegment.totalWaitingDuration)

    val concatenationTotalWaitingDuration = (secondSegment.totalWaitingDuration > 0,
       delta < 0) match {
      case (true,true) => firstSegment.totalWaitingDuration + secondSegment.totalWaitingDuration + delta
      case (true,false) => firstSegment.totalWaitingDuration + Math.max(0, secondSegment.totalWaitingDuration - delta)
      case (false,true) => firstSegment.totalWaitingDuration - delta
      case (false,false) => firstSegment.totalWaitingDuration
    }


    TimeWindowSegment(
      firstSegment.fromIncluded,
      secondSegment.toIncluded,
      firstSegment.fromIncludedLeavingTime,
      concatenationLeavingTime,
      concatenationTotalWaitingDuration,
      concatenationFTS
    )
  }

  /**
    * This method determines if the newNode can be inserted/moved between the previousNode and the nextNode.
    *
    * To do that we consider the inserted node as a segment start and ending at the same node.
    * Then we use the concatenation method to concatenate :
    *   depot -> previousNode + newNode -> new Node + nextNode -> endOfRoute
    *
    * If the resulting FTS of depot on the total segment is negative => not allowed
    *
    * @param previousNode The node after which we insert the newNode
    * @param newNode  The node we want to insert
    * @param nextNode The optional node before which we insert the newNode
    * @return
    */
  private def isNodeInsertionAllowed(previousNode: Int, newNode: Int, nextNode: Option[Int], vehicle: Int): Boolean = {
    // Computing the travel duration : previousNode -> newNode -> nextNode
    println("earlyline previous node : " + earlylines(previousNode))
    val travelToNew = travelTimeMatrix.getTravelDuration(previousNode, leaveTimeAtNodeAtCheckPoint(previousNode), newNode)
    println("travelToNew : " + travelToNew)
    val arrivalAtNew = leaveTimeAtNodeAtCheckPoint(previousNode) + travelToNew
    println("arrivalAtNew : " + arrivalAtNew)
    val waitingDurationAtNew = Math.max(0, earlylines(newNode) - arrivalAtNew)
    println("waitingDurationAtNew : " + waitingDurationAtNew)
    val leaveTimeAtNew = arrivalAtNew + waitingDurationAtNew + taskDurations(newNode)


    val firstSegment: TimeWindowSegment = timeWindowSegmentAtCheckPoint(vehicle)(previousNode)

    val insertedNodeSegment: TimeWindowSegment = timeWindowSegmentAtCheckPoint(newNode)(newNode)

    if(insertedNodeSegment.forwardTimeSlack < 0) return false

    val concatenatedSegment: TimeWindowSegment = concatForwardTimeSlack(firstSegment, insertedNodeSegment)

    if(concatenatedSegment.forwardTimeSlack < 0) return false

    if(nextNode.isDefined){
      val lastSegment: TimeWindowSegment = timeWindowSegmentAtCheckPoint(nextNode.get)(vehicle)
      val fullSegment: TimeWindowSegment = concatForwardTimeSlack(concatenatedSegment, lastSegment)

      return fullSegment.forwardTimeSlack >= 0
    } else {
      return concatenatedSegment.toIncludedLeavingTime <= deadlines(vehicle)
    }

    true
  }

  private def isSegmentInsertOrMoveAllowed(fromIncluded: Int, toIncluded: Int,
                                            newPrevious: Int, newNext: Option[Int],
                                            oldPrevious: Int, oldNext: Option[Int],
                                            fromVehicle: Int, toVehicle: Int,
                                           forwardMove: Boolean): Boolean ={

    (forwardMove, fromVehicle == toVehicle) match{
      case (true,true) => {
        val firstSegment = timeWindowSegmentAtCheckPoint(fromVehicle)(oldPrevious)
        // oldNext should be define otherwise it's not a forward move
        val middleSegment = timeWindowSegmentAtCheckPoint(oldNext.get)(newPrevious)
        val movingSegment = timeWindowSegmentAtCheckPoint(fromIncluded)(toIncluded)
        val concatenateSegment =
          concatForwardTimeSlack(
            concatForwardTimeSlack(firstSegment,middleSegment),
            movingSegment)
        if(newNext.isDefined) {
          val lastSegment = timeWindowSegmentAtCheckPoint(newNext.get)(fromVehicle)
          val fullSegment = concatForwardTimeSlack(concatenateSegment, lastSegment)
          fullSegment.forwardTimeSlack > 0
        } else {
          concatenateSegment.toIncludedLeavingTime <= deadlines(fromVehicle)
        }
      }
      case (false,true) => {
        val firstSegment = timeWindowSegmentAtCheckPoint(fromVehicle)(newPrevious)
        val movingSegment = timeWindowSegmentAtCheckPoint(fromIncluded)(toIncluded)
        // newNext should be define otherwise it's not a backward move
        val middleSegment = timeWindowSegmentAtCheckPoint(newNext.get)(oldPrevious)
        val concatenateSegment =
          concatForwardTimeSlack(
            concatForwardTimeSlack(firstSegment,movingSegment),
            middleSegment)
        if(oldNext.isDefined) {
          val lastSegment = timeWindowSegmentAtCheckPoint(oldNext.get)(fromVehicle)
          val fullSegment = concatForwardTimeSlack(concatenateSegment, lastSegment)
          fullSegment.forwardTimeSlack > 0
        } else {
          concatenateSegment.toIncludedLeavingTime <= deadlines(fromVehicle)
        }
      }
      case (_,false) => {
        val firstSegment = timeWindowSegmentAtCheckPoint(toVehicle)(newPrevious)
        val movingSegment = timeWindowSegmentAtCheckPoint(fromIncluded)(toIncluded)
        val concatenateSegment = concatForwardTimeSlack(firstSegment, movingSegment)

        if(concatenateSegment.forwardTimeSlack < 0) return false

        if(newNext.isDefined) {
          val lastSegment = timeWindowSegmentAtCheckPoint(newNext.get)(toVehicle)
          val fullSegment = concatForwardTimeSlack(concatenateSegment, lastSegment)
          fullSegment.forwardTimeSlack > 0
        } else {
          concatenateSegment.toIncludedLeavingTime <= deadlines(fromVehicle)
        }
      }
    }
  }

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
    println("in update all invalid ...")
    println("changed vehicles ? : " + changedVehicleSinceCheckpoint.toList)
    while(changedVehicleSinceCheckpoint != null){
      val vehicle = changedVehicleSinceCheckpoint.head
      changedVehicleSinceCheckpoint = changedVehicleSinceCheckpoint.tail
      violationAtCheckpoint(vehicle) = doUpdatePrecomputationToCheckpointAndSaveCheckpoint(vehicle)
    }
  }

  def doUpdatePrecomputationToCheckpointAndSaveCheckpoint(vehicle:Int):Int = {
    vehicleChangedSinceCheckpoint(vehicle) = false
    var explorerAtCurrentNode = checkpoint.explorerAtAnyOccurrence(vehicle).head.next
    var previousNode = vehicle
    var exploredNodes: List[Int] = List(vehicle)
    timeWindowSegmentAtCheckPoint(vehicle)(vehicle) =
      TimeWindowSegment(vehicle, vehicle,
        earlylines(vehicle), earlylines(vehicle),
        0, deadlines(vehicle) - earlylines(vehicle))

    def updateForwardTimeSlack(node: Int): Unit ={
      println("updating fts ending at : " + node)

      // Update forward time slack (all subsequence reaching node)
      for(explNode <- exploredNodes){
        timeWindowSegmentAtCheckPoint(explNode)(node) =
          Math.min(
            timeWindowSegmentAtCheckPoint(explNode)(previousNode),
            deadlines(node) - leaveTimeAtNodeAtCheckPoint(node) +
              (if(node < v)
                totalWaitingDurationUntilNodeAtCheckPoint(previousNode) - totalWaitingDurationUntilNodeAtCheckPoint(explNode)
              else
                totalWaitingDurationUntilNodeAtCheckPoint(node) - totalWaitingDurationUntilNodeAtCheckPoint(explNode)
                )
          )
        println("explored node : " + explNode)
        println("to node : " + node)
        println("smaller size fts : " + timeWindowSegmentAtCheckPoint(explNode)(previousNode))
        println("other choice was : " + (deadlines(node) - leaveTimeAtNodeAtCheckPoint(node) +
          (if(node < v)
            totalWaitingDurationUntilNodeAtCheckPoint(previousNode) - totalWaitingDurationUntilNodeAtCheckPoint(explNode)
          else
            totalWaitingDurationUntilNodeAtCheckPoint(node) - totalWaitingDurationUntilNodeAtCheckPoint(explNode)
            )))
        println("now : " + timeWindowSegmentAtCheckPoint(explNode)(node))

      }
    }

    while(explorerAtCurrentNode match{
      case None => // Empty route
        updateForwardTimeSlack(vehicle)
        return if(timeWindowSegmentAtCheckPoint(vehicle)(vehicle) >= 0) 0 else 1
        false
      case Some(nodePosition) =>
        val node = if(nodePosition.value < v) vehicle else nodePosition.value

        if(node >= v) {
          // Update leaveTime value
          val leaveTimeAtPreviousNode = leaveTimeAtNodeAtCheckPoint(previousNode)
          val arrivalTimeAtNode = leaveTimeAtPreviousNode + travelTimeMatrix.getTravelDuration(previousNode, leaveTimeAtPreviousNode, node)
          val leaveTimeAtNode = Math.max(arrivalTimeAtNode, earlylines(node)) + taskDurations(node)
          leaveTimeAtNodeAtCheckPoint(node) = leaveTimeAtNode

          // Update total waiting until node
          val waitingAtNode = earlylines(node) - arrivalTimeAtNode
          totalWaitingDurationUntilNodeAtCheckPoint(node) = totalWaitingDurationUntilNodeAtCheckPoint(previousNode) + waitingAtNode
        }

        // Update FTS
        updateForwardTimeSlack(node)

        if(node >= v) {
          // Add new FTS subsequence (starting and ending at node)
          timeWindowSegmentAtCheckPoint(node)(node) = deadlines(node) - leaveTimeAtNodeAtCheckPoint(node)
        }

        exploredNodes = exploredNodes :+ node
        previousNode = node
        explorerAtCurrentNode = nodePosition.next

        if(node < v){
          return if(timeWindowSegmentAtCheckPoint(vehicle)(vehicle) > 0) 0 else 1
          false
        } else {
          true
        }
    }){}
    throw new Error("unexpected end")
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
        println(value, vehicle)
        println(prevNode, value, nextNode, vehicle)

        println("is insertion valid ? : " + isNodeInsertionAllowed(prevNode, value, nextNode, vehicle))

        if(!isNodeInsertionAllowed(prevNode, value, nextNode, vehicle))
          violations(vehicle) := 1

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
          println("is move accepted ? : " + isNodeInsertionAllowed(prevNode, fromIncludedNode, nextNode, toVehicle))
          if(!isNodeInsertionAllowed(prevNode, fromIncludedNode, nextNode, toVehicle))
            violations(toVehicle) := 1

          return true
        } else {
          if(flip) return false

          if(!isSegmentInsertOrMoveAllowed(prevNode, fromIncludedNode, toIncludedNode, nextNode, toVehicle))
            violations(toVehicle) := 1
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
      //println(violations.toList)
      //println(leaveTimeAtNodeAtCheckPoint.toList.zipWithIndex.filter(x => x._2 == 13 || x._2 == 11).take(v).mkString("\n"))
      //println(forwardTimeSlackAtCheckPoint.map(_.toList).toList.take(v).mkString("\n"))
      c.check(violations(vehicle).value == violationOnVehicleFromScratch(vehicle,seq),
        Some("violationPerVehicle(vehicle).value=" + violations(vehicle).value
          + " should == violationOnVehicle(vehicle,seq)="+  violationOnVehicleFromScratch(vehicle,seq) + " vehicle:" + vehicle + " route:" + seq))

      val oldUpToDate = vehicleChangedSinceCheckpoint(vehicle)
      vehicleChangedSinceCheckpoint(vehicle) = true
      c.check(violations(vehicle).value == violationOnVehicleFromScratch(vehicle,seq))
      vehicleChangedSinceCheckpoint(vehicle) = oldUpToDate
    }
/*
    if(checkpoint != null){
      for(vehicle <- vehicles){
        println("checking " + vehicle)
        val oldUpToDate = vehicleChangedSinceCheckpoint(vehicle)
        vehicleChangedSinceCheckpoint(vehicle) = true
        c.check(violationAtCheckpoint(vehicle) == violationOnVehicleFromScratch(vehicle,checkpoint))
        vehicleChangedSinceCheckpoint(vehicle) = oldUpToDate
      }
    }*/
  }

}


private case class TimeWindowSegment(
                                    fromIncluded: Int,
                                    toIncluded: Int,
                                    fromIncludedLeavingTime: Int,
                                    toIncludedLeavingTime: Int,
                                    totalWaitingDuration: Int,
                                    forwardTimeSlack: Int
                                    )