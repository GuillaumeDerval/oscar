package oscar.cbls.business.routing.invariants.timeWindow

import oscar.cbls._
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.invariants.group._
import oscar.cbls.core._
import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue}

object TimeWindowConstraintWithLogReduction {

  /**
    * This method instantiate a TimeWindow constraint given the following input
    * @param routes The route of the problem (ChangingSeqValue created in the VRP class)
    * @param n The number of nodes of the problem (including vehicle)
    * @param v The number of vehicles of the problem
    * @param earliestArrivalTime An array representing the earliest arrival time at a node (or vehicle's depot)
    * @param latestLeavingTime An array representing the latest leave time at a node (or vehicle's depot)
    * @param travelTimeMatrix A matrix representing the different travel time between the nodes
    * @param violations An array of CBLSIntVar maintaining the violation of each vehicle
    * @return a time window constraint
    */
  def apply(routes: ChangingSeqValue,
            n: Int,
            v: Int,
            earliestArrivalTime: Array[Int],
            latestLeavingTime: Array[Int],
            travelTimeMatrix: Array[Array[Int]],
            violations: Array[CBLSIntVar]): TimeWindowConstraintWithLogReduction ={

    new TimeWindowConstraintWithLogReduction(routes: ChangingSeqValue, n, v,
      earliestArrivalTime,
      latestLeavingTime,
      earliestArrivalTime,
      latestLeavingTime,
      travelTimeMatrix, violations)
  }

  /**
    * This method instantiate a TimeWindow constraint given the following input.
    * @param routes The route of the problem (ChangingSeqValue created in the VRP class)
    * @param n The number of nodes of the problem (including vehicle)
    * @param v The number of vehicles of the problem
    * @param earliestArrivalTime An array (size n) representing the earliest arrival time at a node (or vehicle's depot)
    * @param latestLeavingTime An array (size n) representing the latest leaving time at a node (or vehicle's depot)
    * @param taskDurations An array (size n) representing the task duration at a node (or vehicle's depot)
    * @param travelTimeMatrix A matrix representing the different travel time between the nodes
    * @param violations An array of CBLSIntVar maintaining the violation of each vehicle
    * @return a time window constraint
    */
  def apply(routes: ChangingSeqValue,
            n: Int,
            v: Int,
            earliestArrivalTime: Array[Int],
            latestLeavingTime: Array[Int],
            taskDurations: Array[Int],
            travelTimeMatrix: Array[Array[Int]],
            violations: Array[CBLSIntVar]): TimeWindowConstraintWithLogReduction ={

    new TimeWindowConstraintWithLogReduction(routes: ChangingSeqValue, n, v,
      earliestArrivalTime,
      (latestLeavingTime, taskDurations).zipped.map(_ - _),
      (earliestArrivalTime, taskDurations).zipped.map(_ + _),
      latestLeavingTime,
      travelTimeMatrix, violations)
  }
}

class TimeWindowConstraintWithLogReduction(routes: ChangingSeqValue,
                                           n: Int,
                                           v: Int,
                                           earliestArrivalTime: Array[Int],
                                           latestArrivalTime: Array[Int],
                                           earliestLeavingTime: Array[Int],
                                           latestLeavingTime: Array[Int],
                                           travelTimeMatrix: Array[Array[Int]],
                                           violations: Array[CBLSIntVar]) extends LogReducedGlobalConstraintWithExtremes [TransferFunction, Boolean](routes,v){

  private val transferFunctionOfNode: Array[TransferFunction] = Array.tabulate(n)(
    node =>
      DefinedTransferFunction(
        earliestArrivalTime(node),
        latestArrivalTime(node),
        earliestLeavingTime(node),node,node))

  private def computeLeavingTime(previousLeavingTime: Int, fromNode: Int, nextTransferFunction: TransferFunction): Int ={
    if(nextTransferFunction.isEmpty) return -1
    val toNode = nextTransferFunction.from
    val travelDuration = travelTimeMatrix(fromNode)(toNode)
    nextTransferFunction(previousLeavingTime + travelDuration)
  }

  /**
    * This method makes the composition of two TransferFunction
    * @param f1 The first TransferFunction
    * @param f2 The second TransferFunction
    * @param m The distance between the two TransferFunction
    * @return The composed TransferFunction or an EmptyTransferFunction
    */
  private def composeFunction (f1: TransferFunction, f2: TransferFunction, m: Int): TransferFunction ={
    if(f1.isEmpty)
      return f1
    else if(f2.isEmpty)
      return f2

    val earliestArrivalTimeAt2 = f1.el + m
    val latestArrivalTimeAt2 =
      if(f1.la + f1.el - f1.ea + m < 0)
        Int.MaxValue
      else f1.la + f1.el - f1.ea + m

    val earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 = earliestArrivalTimeAt2 <= f2.ea
    val earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 = earliestArrivalTimeAt2 <= f2.la
    val latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 = latestArrivalTimeAt2 <= f2.ea
    val latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 = latestArrivalTimeAt2 <= f2.la

    val (ea3, la3, el3) =
      (earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
        earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2,
        latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
        latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2) match{
        case (true,true,true,true) =>
          (f1.la, f1.la, f2.el)                                    // e3 == d1 because latest arrival time at 2 is lower than earliest starting time at 2
        // so it doesn't matter when you arrive at 1 the resulting leaving time at 2 will be l2
        // => e3 == d1 (the formula says if (t <= e) => l
        case (true,true,false,true) =>
          (f2.ea - f1.el - m + f1.ea, f1.la, f2.el)
        case (true,true,false,false) =>
          (f2.ea - f1.el - m + f1.ea, f2.la - f1.el - m + f1.ea, f2.el)
        case (false,true,false,true) =>
          (f1.ea, f1.la,f1.el + f2.el - f2.ea + m)
        case (false,true,false,false) =>
          (f1.ea, f2.la - f1.el - m + f1.ea, f1.el + f2.el - f2.ea + m)
        case (false,false,false,false) =>
          (1, -1, -1)
        case _ =>
          throw new Error("Unhandled case : " + (earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
            earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2,
            latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
            latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2))
      }

    if(ea3 > la3)
      EmptyTransferFunction
    else
      DefinedTransferFunction(ea3, la3, el3, f1.from, f2.to)
  }

  /**
    * this method is for composing steps into bigger steps.
    *
    * @param firstStep  the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @param secondStep the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @return the type T associated wit hthe first step followed by the second step
    */
  override def composeSteps(firstStep: TransferFunction, secondStep: TransferFunction): TransferFunction = {
    val firstStepEndsAt = firstStep.to
    val secondStepStartsAt = secondStep.from
    if(firstStepEndsAt == -1 || secondStepStartsAt == -1) EmptyTransferFunction
    else if(firstStepEndsAt == secondStepStartsAt)
      DefinedTransferFunction(firstStep.ea,firstStep.la,secondStep.el,firstStep.from,secondStep.to)
    else {
      val travelDuration = travelTimeMatrix(firstStepEndsAt)(secondStepStartsAt)
      composeFunction(firstStep, secondStep, travelDuration)
    }
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle  the vehicle that we are focusing on
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @return the value associated with the vehicle. This value should only be computed based on the provided segments
    */
  override def computeVehicleValueComposed(vehicle: Int, segments: List[LogReducedSegment[TransferFunction]]): Boolean = {

    def composeTransferFunctions(transferFunctions: QList[TransferFunction], previousLeavingTime: Int, lastNode: Int): Int ={
      val currentTransferFunction = transferFunctions.head
      if(currentTransferFunction.isEmpty) -1
      else {
        val newLeavingTime = computeLeavingTime(previousLeavingTime, lastNode, currentTransferFunction)
        if (transferFunctions.tail == null || newLeavingTime < 0)
          newLeavingTime
        else
          composeTransferFunctions(transferFunctions.tail, newLeavingTime, currentTransferFunction.to)
      }
    }

    def composeLogReduceSegments(logReducedSegments: List[LogReducedSegment[TransferFunction]],
                                 lastNode: Int = vehicle,
                                 previousLeavingTime: Int = 0): Int ={
      logReducedSegments match {
        case Nil => previousLeavingTime
        case head::tail =>
          val (newLastNode, newLeavingTime): (Int,Int) = head match{
            case s@LogReducedPreComputedSubSequence(_, endNode, steps) =>
              (endNode,composeTransferFunctions(steps, previousLeavingTime, lastNode))

            case s@LogReducedFlippedPreComputedSubSequence(_, endNode, steps) =>
              (endNode,composeTransferFunctions(steps, previousLeavingTime, lastNode))

            case s@LogReducedNewNode(node, transferFunctionOfNode) =>
              (node, computeLeavingTime(previousLeavingTime, lastNode, transferFunctionOfNode))
          }
          if(newLeavingTime >= 0)
            composeLogReduceSegments(tail, newLastNode, newLeavingTime)
          else
            newLeavingTime
      }
    }
    composeLogReduceSegments(segments) < 0
  }

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    * @param value   the value of the vehicle
    */
  override def assignVehicleValue(vehicle: Int, value: Boolean): Unit = {
    if(value) violations(vehicle) := 1 else violations(vehicle) := 0
  }

  /**
    * this method is defined for verification purpose. It computes the value of the vehicle from scratch.
    *
    * @param vehicle the vehicle on which the value is computed
    * @param routes  the sequence representing the route of all vehicle
    * @return the value of the constraint for the given vehicle
    */
  override def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): Boolean = {
    var arrivalTimeAtFromNode = earliestArrivalTime(vehicle)
    var leaveTimeAtFromNode = earliestLeavingTime(vehicle)
    var fromNode = vehicle
    val explorerAtVehicleStart = routes.explorerAtAnyOccurrence(vehicle).head
    var explorerAtCurrentNode = explorerAtVehicleStart.next
    var violationFound = false

    while(explorerAtCurrentNode.isDefined && explorerAtCurrentNode.get.value >= v && !violationFound){
      val toNode = explorerAtCurrentNode.get.value
      val travelDuration = travelTimeMatrix(fromNode)(toNode)
      val arrivalTimeAtToNode = leaveTimeAtFromNode + travelDuration
      val leaveTimeAtToNode = Math.max(earliestArrivalTime(toNode), arrivalTimeAtToNode) + earliestLeavingTime(toNode) - earliestArrivalTime(toNode)

      // Check violation
      if(leaveTimeAtToNode > latestLeavingTime(toNode))
        violationFound = true

      // Update values
      fromNode = toNode
      explorerAtCurrentNode = explorerAtCurrentNode.get.next
      arrivalTimeAtFromNode = arrivalTimeAtToNode
      leaveTimeAtFromNode = leaveTimeAtToNode
    }

    // Check travel back to depot
    val travelBackToDepot = travelTimeMatrix(fromNode)(vehicle)
    val arrivalTimeAtDepot = leaveTimeAtFromNode + travelBackToDepot
    violationFound || arrivalTimeAtDepot >= latestLeavingTime(vehicle)
  }

  override def outputVariables: Iterable[Variable] = violations

  /**
    * this method delivers the value of the node
    *
    * @return the type T associated with the node "node"
    */
  override def nodeValue(node: Int): TransferFunction = transferFunctionOfNode(node)

  /**
    * this one is similar to the nodeValue except that it only is applied on vehicle,
    * to represent the return to the vehicle start at teh end of its route
    *
    * @param vehicle
    * @return
    */
  override def endNodeValue(vehicle: Int): TransferFunction = transferFunctionOfNode(vehicle)
}


