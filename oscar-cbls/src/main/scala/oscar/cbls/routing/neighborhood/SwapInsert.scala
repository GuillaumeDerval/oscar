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
 *     This code has been initially developed by Ghilain Florent.
 *     Refactored in respect with the new architecture by Yoann Guyot.
 * ****************************************************************************
 */

package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model._
import oscar.cbls.search.combinators.AndThen
import oscar.cbls.search.move.Move

import scala.collection.immutable.SortedSet

/**
 * inserts a non routed point, and removes a routed point from the same route
 *
 * The search complexity is O(n²).
 * @author yoann.guyot@cetic.be
 *
 *         THIS IS EXPERIMENTAL
 */
case class SwapInsert(unroutedNodesToInsert:()=>Iterable[Int],
                      relevantNeighbors:()=>Int=>Iterable[Int],
                      val vrp: VRP with NodesOfVehicle,
                      val neighborhoodName:String = null,
                      val best:Boolean = false,
                      var insertionPoints:SortedSet[Int] = null)
  extends AndThen(
    new InsertPoint(unroutedNodesToInsert, relevantNeighbors, vrp, "SwapInsert.Insert"),
    new RemovePoint(() => insertionPoints,vrp, "SwapInsert.Remove", false, false)){

  /** this method is called by AndThen to notify the first step, and that it is now exploring successors of this step.
    * this method is called before the step is actually taken.
    * @param m
    */
  override def notifyFirstStep(m: Move){
    m match{
      case i:InsertPointMove =>
        val vehicle = vrp.routeNr(i.beforeInsertedPoint).value
        insertionPoints = vrp.nodesOfVehicle(vehicle).value
    }
  }
}