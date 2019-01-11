package oscar.cbls.business.routing.invariants.timeWindow

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.invariants.group._
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
            violations: Array[CBLSIntVar]): TimeWindowConstraint ={

    new TimeWindowConstraint(routes: ChangingSeqValue, n, v,
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
            violations: Array[CBLSIntVar]): TimeWindowConstraint ={

    new TimeWindowConstraint(routes: ChangingSeqValue, n, v,
      earliestArrivalTime,
      (latestLeavingTime, taskDurations).zipped.map(_ - _),
      (earliestArrivalTime, taskDurations).zipped.map(_ + _),
      latestLeavingTime,
      travelTimeMatrix, violations)
  }
}

/**
  * This class represent a time window constraint.
  * Given parameters, it maintains the violation value of each vehicle (in violations var)
  * @param routes The route of the problem (ChangingSeqValue created in the VRP class)
  * @param n The number of nodes of the problem (including vehicle)
  * @param v The number of vehicles of the problem
  * @param earliestArrivalTime An array (size n) representing the earliest arrival time at a node (or vehicle's depot)
  * @param latestArrivalTime An array (size n) representing the latest arrival time at a node (or vehicle's depot)
  * @param earliestLeavingTime An array (size n) representing the earliest leaving time at a node (or vehicle's depot)
  * @param latestLeavingTime An array (size n) representing the latest leaving time at a node (or vehicle's depot)
  * @param travelTimeMatrix A matrix representing the different travel time between the nodes
  * @param violations An array of CBLSIntVar maintaining the violation of each vehicle
  */
class TimeWindowConstraint (routes: ChangingSeqValue,
                            n: Int,
                            v: Int,
                            earliestArrivalTime: Array[Int],
                            latestArrivalTime: Array[Int],
                            earliestLeavingTime: Array[Int],
                            latestLeavingTime: Array[Int],
                            travelTimeMatrix: Array[Array[Int]],
                            violations: Array[CBLSIntVar]
                           ) extends GlobalConstraintDefinition[Array[TransferFunction], Boolean](routes, v) with SeqNotificationTarget {

  private val transferFunctionOfNode: Array[TransferFunction] = Array.tabulate(n)(
    node =>
      DefinedTransferFunction(
        earliestArrivalTime(node),
        latestArrivalTime(node),
        earliestLeavingTime(node),node,node))

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

    val (ea3, ll3, el3) =
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

    if(ea3 > ll3)
      EmptyTransferFunction
    else
      DefinedTransferFunction(ea3, ll3, el3, f1.from, f2.to)
  }

  private def segmentsInfo(segment: Segment[Array[TransferFunction]]): (Int, Int, TransferFunction) ={
    segment match{
      case seg: PreComputedSubSequence[Array[TransferFunction]] =>
        (seg.startNode, seg.endNode, seg.startNodeValue(seg.endNode))
      case seg: FlippedPreComputedSubSequence[Array[TransferFunction]] => (seg.startNode, seg.endNode, seg.startNodeValue(seg.endNode))
      case seg: NewNode[Array[TransferFunction]] => (seg.node, seg.node, transferFunctionOfNode(seg.node))
    }
  }

  /**
    * tis method is called by the framework when a pre-computation must be performed.
    * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
    *
    * @param vehicle         the vehicle where pre-computation must be performed
    * @param routes          the sequence representing the route of all vehicle
    *                        BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    * @param preComputedVals The array of precomputed values
    */
  override def performPreCompute(vehicle: Int, routes: IntSequence, preComputedVals: Array[Array[TransferFunction]]): Unit = {
    def performPreComputeOnRoute(route: List[Int]): Unit ={
      val node = route.head
      if(preComputedVals(node) == null)preComputedVals(node) = Array.fill(n)(EmptyTransferFunction)
      preComputedVals(node)(node) = transferFunctionOfNode(node)
      var lastTF = preComputedVals(node)(node)
      var prevNode = node
      for(curNode <- route.tail){
        val newTF = if(lastTF.isEmpty) lastTF else composeFunction(lastTF, transferFunctionOfNode(curNode), travelTimeMatrix(prevNode)(curNode))
        preComputedVals(node)(curNode) = newTF
        prevNode = curNode
        lastTF = newTF
      }
      if(route.size > 1)
        performPreComputeOnRoute(route.tail)
    }

    var continue = true
    var vExplorer = routes.explorerAtAnyOccurrence(vehicle)
    var route: List[Int] = List.empty
    while(continue){
      vExplorer match {
        case None => continue = false
        case Some(elem) =>
          if (elem.value < v && elem.value != vehicle){
            continue = false
          } else {
            route = elem.value :: route
          }
          vExplorer = elem.next
      }
    }

    performPreComputeOnRoute(route)
    performPreComputeOnRoute(route.reverse)
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle         the vehicle that we are focusing on
    * @param segments        the segments that constitute the route.
    *                        The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @param routes          the sequence representing the route of all vehicle
    * @param preComputedVals The array of precomputed values
    * @return the value associated with the vehicle
    */
  override def computeVehicleValue(vehicle: Int, segments: List[Segment[Array[TransferFunction]]], routes: IntSequence, preComputedVals: Array[Array[TransferFunction]]): Boolean = {
    /**
      * @param segments The list of segment
      * @param prevLeavingTime The leave time at previous segment (0 if first one)
      * @return The leave time after going through all the segments
      */
    def arrivalAtDepot(segments: Iterator[Segment[Array[TransferFunction]]], previousSegmentEnd: Int = vehicle, prevLeavingTime: Int = 0): Int ={
      val segment = segments.next
      val (segmentStart, segmentEnd, transferFunction) = segmentsInfo(segment)
      val arrivalTimeAtSegment = prevLeavingTime + travelTimeMatrix(previousSegmentEnd)(segmentStart)
      val leaveTimeAtSegment = transferFunction(arrivalTimeAtSegment)
      if(leaveTimeAtSegment >= 0) {
        if (segments.hasNext)
          arrivalAtDepot(segments, segmentEnd, leaveTimeAtSegment)
        else
          leaveTimeAtSegment + travelTimeMatrix(segmentEnd)(vehicle)
      }
      else leaveTimeAtSegment
    }

    val arrivalTimeAtDepot = arrivalAtDepot(segments.toIterator)
    arrivalTimeAtDepot < 0 || arrivalTimeAtDepot > latestLeavingTime(vehicle)
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
    * This method is defined for verification purpose.
    * It computes the value of the vehicle from scratch.
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

  override def outputVariables: Iterable[Variable] = {
    violations
  }

  /**
    * Return all transfer functions of the problem.
    * Meant for post-optimisation. Don't use it during optimisation.
    * @return A 2-dimension table of TransferFunction
    */
  def transferFunctions(routes: IntSequence): Array[Array[TransferFunction]] ={
    val transferFunctions: Array[Array[TransferFunction]] = Array.fill(n)(Array.fill(n)(EmptyTransferFunction))
    for(curV <- 0 to v){
      performPreCompute(curV, routes, transferFunctions)
    }

    transferFunctions
  }
}
