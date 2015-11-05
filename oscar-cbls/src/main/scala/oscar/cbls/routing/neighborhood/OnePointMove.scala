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
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer and Florent Ghilain.
 *     Factorization of code by Yoann Guyot.
 * ****************************************************************************
 */

package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model.PositionInRouteAndRouteNr
import oscar.cbls.routing.model.VRP
import oscar.cbls.search.algo.HotRestart

/**
 * Moves a point of a route to another place in the same or in an other route.
 * The search complexity is O(nk).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class OnePointMove(nodesPrecedingNodesToMove: () => Iterable[Int],
                        relevantNeighbors: () => Int => Iterable[Int],
                        vrp: VRP with PositionInRouteAndRouteNr,
                        neighborhoodName: String = null,
                        best: Boolean = false,
                        hotRestart: Boolean = true) extends EasyRoutingNeighborhood(best, vrp, neighborhoodName) {

  //the indice to start with for the exploration
  var startIndice: Int = 0

  override def exploreNeighborhood() {

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(nodesPrecedingNodesToMove(), startIndice)
      else nodesPrecedingNodesToMove()

    cleanRecordedMoves()

    val relevantNeighborsNow = relevantNeighbors()

    for (
      beforeMovedPoint <- iterationSchemeOnZone if vrp.isRouted(beforeMovedPoint)
    ) {

      val movedPoint = vrp.next(beforeMovedPoint).value

      for (
        insertionPoint <- relevantNeighborsNow(movedPoint) if (vrp.isRouted(insertionPoint)
          && beforeMovedPoint != insertionPoint
          && movedPoint != insertionPoint
          && beforeMovedPoint != vrp.next(insertionPoint).value)
          && (!vrp.isADepot(movedPoint) || vrp.onTheSameRoute(movedPoint, insertionPoint))
      ) {

        encode(beforeMovedPoint, insertionPoint)
        val newObj = evalObjOnEncodedMove()

        if (moveRequested(newObj)
          && submitFoundMove(OnePointMoveMove(beforeMovedPoint, movedPoint, insertionPoint, newObj, this, neighborhoodNameToString))) {
          startIndice = beforeMovedPoint + 1
          return
        }
      }
    }
  }

  override def reset(): Unit = {
    startIndice = 0
  }

  def encode(predOfMovedPoint: Int, insertionPoint: Int) {
    val s = cutNodeAfter(predOfMovedPoint)
    insert(s, insertionPoint)
  }
}

/**
 * Models a one-point-move operator of a given VRP problem.
 * @param predOfMovedPoint the predecessor of the point that moves.
 * @param insertionPoint the place where insert the moving point.
 * @param objAfter the objective value if we performed this one-point-move operator.
 * @param neighborhood the originating neighborhood
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class OnePointMoveMove(predOfMovedPoint: Int,
                            movedPoint: Int,
                            insertionPoint: Int,
                            override val objAfter: Int,
                            override val neighborhood: OnePointMove,
                            override val neighborhoodName: String = null) extends VRPMove(objAfter, neighborhood, neighborhoodName) {

  override def encodeMove() {
    neighborhood.encode(predOfMovedPoint, insertionPoint)
  }

  override def toString: String = (
    neighborhoodNameToString + "OnePointMove(Moved point " + movedPoint
    + " after " + insertionPoint + objToString + " )")
}

