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
 *     Refactored with respect to the new architecture by Yoann Guyot.
 * ****************************************************************************
 */

package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model._
import oscar.cbls.search.algo.{IdenticalAggregator, HotRestart}

/**
 * Inserts an unrouted point in a route. The size of the neighborhood is O(u*n).
 * where u is the numberof unrouted points, and n is the number of routed points
 * it can be cut down to u*k by using the relevant neighbors, and specifying k neighbors for each unrouted point
 * @param unroutedNodesToInsert the nodes that this neighborhood will try to insert SHOULD BE NOT ROUTED
 * @param relevantNeighbors a function that, for each unrouted node gives a routed node
 *                          such that it is relevant to insert the unrouted node after this routed node
 * @param vrp the routing problem
 * @param neighborhoodName the name of this neighborhood
 * @param best should we search for the best move or the first move?
 * @param hotRestart set to true fo a hot restart fearture
 * @param nodeSymmetryClass a function that input the ID of an unrouted node and returns a symmetry class;
 *                      ony one of the unrouted node in each class will be considered for insert
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
case class InsertPoint(unroutedNodesToInsert: () => Iterable[Int],
                       relevantNeighbors: () => Int => Iterable[Int],
                       vrp: VRP,
                       neighborhoodName: String = null,
                       best: Boolean = false,
                       hotRestart: Boolean = true,
                       nodeSymmetryClass:Option[Int => Int] = None) extends EasyRoutingNeighborhood(best, vrp, neighborhoodName) {

  //the indice to start with for the exploration
  //TODO: this is poor, we should do the hotRestart on the symmetry class instead of the node indice.
  var startIndice: Int = 0

  override def exploreNeighborhood(): Unit = {

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(unroutedNodesToInsert(), startIndice)
      else unroutedNodesToInsert()

    val iterationScheme = nodeSymmetryClass match {
      case None => iterationSchemeOnZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(iterationSchemeOnZone, s)
    }
    cleanRecordedMoves()
    val relevantNeighborsNow = relevantNeighbors()

    for (insertedPoint <- iterationScheme) {
      assert(!vrp.isRouted(insertedPoint),
        "The search zone should be restricted to unrouted nodes when inserting.")

      for (
        beforeInsertedPoint <- relevantNeighborsNow(insertedPoint) if vrp.isRouted(beforeInsertedPoint)
      ) {
        assert(isRecording, "MoveDescription should be recording now")

        encode(beforeInsertedPoint, insertedPoint)
        val newObj = evalObjOnEncodedMove()

        if (moveRequested(newObj)
          && submitFoundMove(InsertPointMove(beforeInsertedPoint, insertedPoint, newObj, this, neighborhoodNameToString))) {
          startIndice = insertedPoint + 1
          return
        }
      }
    }
  }

  def encode(beforeInsertedPoint: Int, insertedPoint: Int) {
    assert(!vrp.isRouted(insertedPoint))
    assert(vrp.isRouted(beforeInsertedPoint))
    val s = segmentFromUnrouted(insertedPoint)
    insert(s, beforeInsertedPoint)
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

/**
 * Models a reinsert-point operator of a given VRP problem.
 * @param beforeInsertedPoint the place where to insert an unrouted point.
 * @param insertedPoint an unrouted point.
 * @param objAfter the objective value if we performed this reinsert-point operator.
 * @param neighborhood the originating neighborhood.
 */
case class InsertPointMove(beforeInsertedPoint: Int,
                           insertedPoint: Int,
                           override val objAfter: Int,
                           override val neighborhood: InsertPoint,
                           override val neighborhoodName: String = null)
  extends VRPMove(objAfter, neighborhood, neighborhoodName) {

  override def encodeMove() {
    neighborhood.encode(beforeInsertedPoint, insertedPoint)
  }

  override def toString: String =
    "InsertPoint(beforeInsertedPoint = " + beforeInsertedPoint +
      ", insertedPoint = " + insertedPoint + " )"
}
