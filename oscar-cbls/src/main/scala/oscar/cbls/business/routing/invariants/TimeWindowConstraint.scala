package oscar.cbls.business.routing.invariants

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.TTFMatrix
import oscar.cbls.business.routing.invariants.group.{GlobalConstraintDefinition, Segment}
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
                             ) extends GlobalConstraintDefinition[(Int) => (Int), Int](routes, v) with SeqNotificationTarget {
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
  override def performPreCompute(vehicle: Int, routes: IntSequence, preComputedVals: Array[Int => Int]): Unit = ???

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
  override def computeVehicleValue(vehicle: Int, segments: List[Segment[Int => Int]], routes: IntSequence, PreComputedVals: Array[Int => Int]): Int = ???

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    * @param value   the value of the vehicle
    */
  override def assignVehicleValue(vehicle: Int, value: Int): Unit = ???

  /**
    *
    * @param vehicle
    * @param routes
    * @return
    */
  override def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): Int = ???

  override def outputVariables: Iterable[Variable] = ???

  private def composeFunction (f1: TransfertFunction, f2: TransfertFunction, m: Int): TransfertFunction ={

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
      new TransfertFunction(Int.MaxValue,Int.MaxValue,Int.MaxValue)
    else
      new TransfertFunction(e3, d3, l3)
  }

  private class TransfertFunction (val e: Int, val d: Int, val l: Int){
    require(d >= e && l >= e)
    def apply(t: Int): Option[Int] ={
      if(t <= e)
        Some(l)
      else if(t <= d)
        Some(t + l - e)
      else
        None
    }

    override def toString: String = {
      "e : " + e + "\n d : " + d + "\n l : " + l
    }
  }
}