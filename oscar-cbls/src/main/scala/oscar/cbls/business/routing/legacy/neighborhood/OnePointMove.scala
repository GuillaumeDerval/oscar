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

package oscar.cbls.business.routing.legacy.neighborhood

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.business.routing.legacy.model.{PositionInRouteAndRouteNr, VRP}

/**
 * Moves a point of a route to another place in the same or in an other route.
 * The search complexity is O(nk).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class OnePointMove(nodesPrecedingNodesToMove: () => Iterable[Int],
                        relevantNeighbors: () => Int => Iterable[Int],
                        override val vrp: VRP with PositionInRouteAndRouteNr,
                        neighborhoodName: String = "OnePointMove",
                        best: Boolean = false,
                        hotRestart: Boolean = true) extends EasyRoutingNeighborhood[OnePointMoveMove](best, vrp, neighborhoodName) {

  //the indice to start with for the exploration
  var startIndice: Int = 0

  override def exploreNeighborhood() {

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(nodesPrecedingNodesToMove(), startIndice)
      else nodesPrecedingNodesToMove()

    cleanRecordedMoves()

    val relevantNeighborsNow = relevantNeighbors()

    val beforeMovedPointsIt = iterationSchemeOnZone.iterator
    while (beforeMovedPointsIt.hasNext) {
      beforeMovedPoint = beforeMovedPointsIt.next()
      if (vrp.isRouted(beforeMovedPoint)) {
        movedPoint = vrp.next(beforeMovedPoint).newValue

        val insertionPointIt = relevantNeighborsNow(movedPoint).iterator
        while (insertionPointIt.hasNext) {
          insertionPoint = insertionPointIt.next()
          if (vrp.isRouted(insertionPoint)
            && beforeMovedPoint != insertionPoint
            && movedPoint != insertionPoint
            && beforeMovedPoint != vrp.next(insertionPoint).newValue
            && (!vrp.isADepot(movedPoint) || vrp.onTheSameRoute(movedPoint, insertionPoint))) {

            encode(beforeMovedPoint, insertionPoint)

            if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjOnEncodedMove())){
              startIndice = beforeMovedPoint + 1
              return
            }
          }
        }
      }
    }
  }

  var beforeMovedPoint:Int = 0
  var movedPoint:Int = 0
  var insertionPoint:Int = 0

  override def instantiateCurrentMove(newObj: Int) =
    OnePointMoveMove(beforeMovedPoint, movedPoint, insertionPoint, newObj, this, neighborhoodName)

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
                            override val neighborhoodName: String = "OnePointMoveMove") extends VRPMove(objAfter, neighborhood, neighborhoodName){

  override def impactedPoints: List[Int] = List(predOfMovedPoint,movedPoint,insertionPoint)

  override def encodeMove() {
    neighborhood.encode(predOfMovedPoint, insertionPoint)
  }

  override def toString: String = (
    neighborhoodNameToString + "OnePointMove(Moved point " + movedPoint
      + " after " + insertionPoint + objToString + ")")
}

