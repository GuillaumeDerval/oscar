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

import oscar.cbls.invariants.core.computation.Snapshot
import oscar.cbls.routing.model._
import oscar.cbls.search.combinators.{AndThen, DynAndThen, DynAndThenWithPrev}
import oscar.cbls.search.core.Neighborhood

import scala.collection.immutable.SortedSet

object SwapInsert {

  /**
   * inserts a non routed point, and removes a routed point
   * but not necessarily at the same place, and not necessarily from the same route,
   * unless you'v specified some searchZone parameters
   * The search complexity is O(n²).
   * @param unroutedNodesToInsert the nodes that this neighborhood will try to insert SHOULD BE NOT ROUTED
   * @param relevantNeighborsForInsertion a function that, for each unrouted node gives a list of routed node
   *                                      such that it is relevant to insert the unrouted node after this routed node
   * @param vrp the routing problem
   * @param neighborhoodName the name of this neighborhood
   * @param best should we search for the best move or the first move?
   * @param symmetryClassesOnInsert a function that input the ID of an unrouted node and returns a symmetry class;
   *                                ony one of the unrouted node in each class will be considered for insert
   *                                Int.MinValue is considered different to itself
   *                                if you set to None this will not be used at all
   *                                , inactive if samePlace
   * @param predecessorsOfRoutedPointsToRemove: the predecessors of the points that we will try to remove, inactive if samePlace
   * @param hotRestartOnInsert set to true fo a hot restart for the insertion, with symmetry elimination if present (hot restart on the point to insert, not the position)
   * @param hotRestartOnRemove true if hotRestart is needed, false otherwise
   * @param maximalIntermediaryDegradation the maximal degradation for the intermediary step. Typically set it to Int.MaxValue-1
   * @author yoann.guyot@cetic.be
   * @author rdl@cetic.be
   *
   *         THIS IS EXPERIMENTAL
   */
  def apply(unroutedNodesToInsert: () => Iterable[Int],
            relevantNeighborsForInsertion: () => Int => Iterable[Int],
            predecessorsOfRoutedPointsToRemove: () => Iterable[Int],
            vrp: VRP,
            neighborhoodName: String = "SwapInsert",
            best: Boolean = false,
            symmetryClassesOnInsert: Option[Int => Int] = None,
            hotRestartOnInsert: Boolean = true,
            hotRestartOnRemove: Boolean = true,
            maximalIntermediaryDegradation: Int = Int.MaxValue): Neighborhood = {
    AndThen(RemovePoint(predecessorsOfRoutedPointsToRemove, vrp, "SwapInsert.Remove", best, hotRestartOnRemove),
      InsertPointUnroutedFirst(unroutedNodesToInsert, relevantNeighborsForInsertion, vrp, "SwapInsert.Insert", best, hotRestartOnInsert, symmetryClassesOnInsert, hotRestartOnInsert),
      maximalIntermediaryDegradation) name neighborhoodName
  }


  def swapInsertSamePlace(unroutedNodesToInsert: Int => Iterable[Int],
                          predecessorsOfRoutedPointsToRemove: () => Iterable[Int],
                          vrp: VRP,
                          neighborhoodName: String = "SwapInsert",
                          best: Boolean = false,
                          symmetryClassesOnInsert: Option[Int => Int] = None,
                          hotRestartOnRemove: Boolean = true,
                          hotRestartOnInsert: Boolean = true,
                          maximalIntermediaryDegradation: Int = Int.MaxValue): Neighborhood = {
    DynAndThen(RemovePoint(predecessorsOfRoutedPointsToRemove, vrp, "SwapInsert.Remove", best, hotRestartOnRemove),
      ((r: RemovePointMove) =>
        InsertPointUnroutedFirst(() => unroutedNodesToInsert(r.removedPoint),
          () => _ => List(r.beforeRemovedPoint),
          vrp,
          "SwapInsert.Insert",
          best,
          hotRestartOnInsert,
          symmetryClassesOnInsert,
          false)),
      maximalIntermediaryDegradation) name neighborhoodName
  }

  def swapInsertSameVehicle(predecessorsOfRoutedPointsToRemove: () => Iterable[Int],
                            unroutedNodesToInsert: Int => Iterable[Int], //which nodedo you want to insert on this vehicle, and close to this point?
                            insertionPoints:SortedSet[Int] => Int => Iterable[Int],//given remaining nodes of the vehice, and an insertion candidate, where do we put it?
                            best: Boolean = false,
                            symmetryClassesOnInsert: Option[Int => Int] = None,
                            hotRestartOnRemove: Boolean = true,
                            vrp:VRP with NodesOfVehicle with PositionInRouteAndRouteNr,
                            neighborhoodName: String = "SwapInsertSameVehicle",
                            maximalIntermediaryDegradation: Int = Int.MaxValue) = {
    DynAndThenWithPrev(RemovePoint(predecessorsOfRoutedPointsToRemove, vrp, "SwapInsertSameVehicle.Remove", best, hotRestartOnRemove),
      (r:RemovePointMove,snapShot:Snapshot) => {
        val removedPoint = r.removedPoint
        val vehicleTraversingRemovedPoint = snapShot.intDico(vrp.routeNr(removedPoint))
        val remainingNodesOfThisVehicle = snapShot.setDico(vrp.nodesOfVehicle(vehicleTraversingRemovedPoint))-removedPoint
        InsertPointUnroutedFirst(()=>unroutedNodesToInsert(vehicleTraversingRemovedPoint),
          () => insertionPoints(remainingNodesOfThisVehicle), //where do you want to insert them?
          vrp,
          "SwapInsertSameVehicle.Insert",
          best,
          false,
          symmetryClassesOnInsert,
          false)},
      maximalIntermediaryDegradation,
      vrp.routeNr,
      vrp.nodesOfVehicle) name neighborhoodName
  }

  def swapInsertSameVehicle2(predecessorsOfRoutedPointsToRemove: () => Iterable[Int],
                            unroutedNodesToInsert: Int => Iterable[Int], //which nodedo you want to insert on this vehicle, and close to this point?
                            insertionPoints:SortedSet[Int] => Int => Iterable[Int],//given remaining nodes of the vehice, and an insertion candidate, where do we put it?
                            best: Boolean = false,
                            symmetryClassesOnInsert: Option[Int => Int] = None,
                            hotRestartOnRemove: Boolean = true,
                            vrp:VRP with NodesOfVehicle with PositionInRouteAndRouteNr,
                            neighborhoodName: String = "SwapInsertSameVehicle",
                            maximalIntermediaryDegradation: Int = Int.MaxValue){
    DynAndThenWithPrev(RemovePoint(predecessorsOfRoutedPointsToRemove, vrp, "SwapInsertSameVehicle.Remove", best, hotRestartOnRemove),
      (r:RemovePointMove,snapShot:Snapshot) => {
        val removedPoint = r.removedPoint
        val vehicleTraversingRemovedPoint = snapShot.intDico(vrp.routeNr(removedPoint))
        val remainingNodesOfThisVehicle = snapShot.setDico(vrp.nodesOfVehicle(vehicleTraversingRemovedPoint))-removedPoint
        InsertPointUnroutedFirst(()=>unroutedNodesToInsert(vehicleTraversingRemovedPoint),
          () => insertionPoints(remainingNodesOfThisVehicle), //where do you want to insert them?
          vrp,
          "SwapInsertSameVehicle.Insert",
          best,
          false,
          symmetryClassesOnInsert,
          false)},
      maximalIntermediaryDegradation,
      vrp.routeNr,
      vrp.nodesOfVehicle) name neighborhoodName
  }
}


