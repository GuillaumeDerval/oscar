package oscar.cbls.business.routing.invariants

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.TTFMatrix
import oscar.cbls.business.routing.invariants.group._
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
            n: Int,
            v: Int,
            timeWindows: TimeWindow,
            travelTimeMatrix: TTFMatrix,
            violations: Array[CBLSIntVar]): TimeWindowConstraint =
    new TimeWindowConstraint(routes: ChangingSeqValue, n, v, timeWindows, travelTimeMatrix, violations)
}

class TimeWindowConstraint (routes: ChangingSeqValue,
                              n: Int,
                              v: Int,
                              timeWindows: TimeWindow,
                              travelTimeMatrix: TTFMatrix,
                              violations: Array[CBLSIntVar]
                             ) extends GlobalConstraintDefinition[Array[TransfertFunction], Int](routes, v) with SeqNotificationTarget {
  private val earlylines = timeWindows.earlylines
  private val taskDurations = timeWindows.taskDurations
  private val deadlines = timeWindows.deadlines.zipWithIndex.map(x => x._1 - taskDurations(x._2))

  private val transfertFunctionOfNode: Array[TransfertFunction] = Array.tabulate(n)(
    node =>
      DefinedTransfertFunction(
        earlylines(node),
        deadlines(node),
        earlylines(node) + taskDurations(node)))

  private def composeFunction (f1: TransfertFunction, f2: TransfertFunction, m: Int): TransfertFunction ={
    if(f1.isEmpty)
      return f1
    else if(f2.isEmpty)
      return f2

    val earliestArrivalTimeAt2 = f1.l + m
    val latestArrivalTimeAt2 = f1.d + f1.l - f1.e + m

    val earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 = earliestArrivalTimeAt2 <= f2.e
    val earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 = earliestArrivalTimeAt2 <= f2.d
    val latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 = latestArrivalTimeAt2 <= f2.e
    val latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 = latestArrivalTimeAt2 <= f2.d

    val (e3, d3, l3) =
      (earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
        earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2,
        latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
        latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2) match{
        case (true,true,true,true) =>
          (f1.d, f1.d, f2.l)
        case (true,true,false,true) =>
          (f2.e - f1.l - m + f1.e, f1.d, f2.l)
        case (false,true,false,true) =>
          (f1.e, f1.d,f1.l + f2.l - f2.e + m)
        case (true,true,false,false) =>
          (f2.e - f1.l - m + f1.e, f2.d - f1.l - m + f1.e, f2.l)
        case (false,true,false,false) =>
          (f1.e, f2.d - f1.l - m + f1.e, f1.l + f2.l - f2.e + m)
        case (false,false,false,false) =>
          (1, -1, -1)
        case _ =>
          throw new Error("Unhandled case : " + (earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
            earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2,
            latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
            latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2))
      }

    if(e3 > d3)
      EmptyTransfertFunction
    else
      DefinedTransfertFunction(e3, d3, l3)
  }

  private def segmentsInfo(segment: Segment[Array[TransfertFunction]]): (Int, Int, TransfertFunction) ={
    segment match{
      case seg: PreComputedSubSequence[Array[TransfertFunction]] =>
        (seg.startNode, seg.endNode, seg.startNodeValue(seg.endNode))
      case seg: FlippedPreComputedSubSequence[Array[TransfertFunction]] => (seg.startNode, seg.endNode, seg.startNodeValue(seg.endNode))
      case seg: NewNode[Array[TransfertFunction]] => (seg.node, seg.node, transfertFunctionOfNode(seg.node))
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
  override def performPreCompute(vehicle: Int, routes: IntSequence, preComputedVals: Array[Array[TransfertFunction]]): Unit = {
    def updatePrevNodes(newNode: Int, prevNodes: List[Int]): Unit ={
      //prevNodes is reversed
      val lastNode = prevNodes.head
      for(node <- prevNodes){
        val fromTF = preComputedVals(node)(lastNode)
        preComputedVals(node)(newNode) = composeFunction(
          fromTF,
          transfertFunctionOfNode(newNode),
          travelTimeMatrix.getTravelDuration(lastNode, fromTF.l, newNode))
      }
    }

    var continue = true
    var vExplorer = routes.explorerAtAnyOccurrence(vehicle)
    var exploredNodes: List[Int] = List()
    while(continue){
      vExplorer match {
        case None => continue = false
        case Some(elem) =>
          if (elem.value < v && elem.value != vehicle){
            continue = false
          } else {
            if(exploredNodes.nonEmpty)
              updatePrevNodes(elem.value, exploredNodes)
            if(preComputedVals(elem.value) == null)preComputedVals(elem.value) = Array.fill(n)(EmptyTransfertFunction)
            preComputedVals(elem.value)(elem.value) = transfertFunctionOfNode(elem.value)
            exploredNodes = elem.value :: exploredNodes
          }
          vExplorer = elem.next
      }
    }
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
  override def computeVehicleValue(vehicle: Int, segments: List[Segment[Array[TransfertFunction]]], routes: IntSequence, preComputedVals: Array[Array[TransfertFunction]]): Int = {
    def concatSegments(segments: List[Segment[Array[TransfertFunction]]], concatenateFunction: TransfertFunction, prevSegment: Segment[Array[TransfertFunction]]): TransfertFunction ={
      if(segments.isEmpty)
        return concatenateFunction

      val lastSegmentInfo = segmentsInfo(prevSegment)
      val currentSegmentInfo = segmentsInfo(segments.head)
      val travelDurationBetweenSegments = travelTimeMatrix.getTravelDuration(lastSegmentInfo._2, concatenateFunction.l, currentSegmentInfo._1)
      val newComposedFunction = composeFunction(concatenateFunction, currentSegmentInfo._3, travelDurationBetweenSegments)

      if(newComposedFunction.isEmpty)
        return newComposedFunction

      concatSegments(segments.tail, newComposedFunction, segments.head)
    }

    val firstSegmentInfo = segmentsInfo(segments.head)
    val transfertFunctionOfConcatenatedSegments =
      concatSegments(segments.tail :+ NewNode[Array[TransfertFunction]](vehicle), firstSegmentInfo._3, segments.head)

    if(transfertFunctionOfConcatenatedSegments.isEmpty) 1 else 0
  }

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    * @param value   the value of the vehicle
    */
  override def assignVehicleValue(vehicle: Int, value: Int): Unit = {
    violations(vehicle) := value
  }

  /**
    * this method is defined for verification purpose. It computes the value of the vehicle from scratch.
    *
    * @param vehicle the vehicle on which the value is computed
    * @param routes  the sequence representing the route of all vehicle
    * @return the value of the constraint for the given vehicle
    */
  override def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): Int = {
    var arrivalTimeAtFromNode = timeWindows.earlylines(vehicle)
    var leaveTimeAtFromNode = timeWindows.earlylines(vehicle)
    var fromNode = vehicle
    val explorerAtVehicleStart = routes.explorerAtAnyOccurrence(vehicle).head
    var explorerAtCurrentNode = explorerAtVehicleStart.next
    var violationFound = false

      while(explorerAtCurrentNode.isDefined && explorerAtCurrentNode.get.value >= v && !violationFound){
        val toNode = explorerAtCurrentNode.get.value
        val travelDuration = travelTimeMatrix.getTravelDuration(fromNode, leaveTimeAtFromNode, toNode)
        val arrivalTimeAtToNode = leaveTimeAtFromNode + travelDuration
        val leaveTimeAtToNode = Math.max(timeWindows.earlylines(toNode), arrivalTimeAtToNode) + timeWindows.taskDurations(toNode)

        // Check violation
        if(leaveTimeAtToNode > timeWindows.deadlines(toNode))
            violationFound = true

//        encounteredNodes = encounteredNodes :+ toNode
//        travelPerformed = travelPerformed :+ travelDuration

        // Update values
        fromNode = toNode
        explorerAtCurrentNode = explorerAtCurrentNode.get.next
        arrivalTimeAtFromNode = arrivalTimeAtToNode
        leaveTimeAtFromNode = leaveTimeAtToNode
    }

    // Check travel back to depot
    val travelBackToDepot = travelTimeMatrix.getTravelDuration(fromNode, leaveTimeAtFromNode, vehicle)
    val arrivalTimeAtDepot = leaveTimeAtFromNode + travelBackToDepot
    if(violationFound || arrivalTimeAtDepot >= timeWindows.deadlines(vehicle)) 1 else 0
  }

  override def outputVariables: Iterable[Variable] = {
    violations
  }
}


abstract class TransfertFunction (val e: Int, val d: Int, val l: Int){

  def apply(t: Int): Option[Int]

  def isEmpty: Boolean

  override def toString: String = {
    "e : " + e + "\n d : " + d + "\n l : " + l
  }
}

case class DefinedTransfertFunction (override val e: Int, override val d: Int, override val l: Int) extends TransfertFunction(e,d,l){
  require(d >= e && l >= e, "e : " + e + ", d : " + d + ", l : " + l)
  override def apply(t: Int): Option[Int] = {
    if(t <= e)
      Some(l)
    else if(t <= d)
      Some(t + l - e)
    else
      None
  }

  override def isEmpty: Boolean = false
}

case object EmptyTransfertFunction extends TransfertFunction(1,-1,-1){
  override def apply(t: Int): Option[Int] = None

  override def isEmpty: Boolean = true
}