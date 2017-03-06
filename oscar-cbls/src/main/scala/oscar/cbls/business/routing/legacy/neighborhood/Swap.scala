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

package oscar.cbls.business.routing.legacy.neighborhood

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.business.routing.legacy.model._

/**
 * Swaps two points of the same or different routes.
 * The search complexity is O(n²).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class Swap(nodesPrecedingNodesToMove:()=>Iterable[Int],
           relevantNeighbors:()=>Int=>Iterable[Int],
           override val  vrp: VRP with PositionInRouteAndRouteNr,
           neighborhoodName:String = null,
           val best:Boolean = false,
           val hotRestart:Boolean = true) extends EasyRoutingNeighborhood[SwapMove](best,vrp,neighborhoodName) {

  //the indice to start with for the exploration
  var startIndice:Int = 0

  override def exploreNeighborhood(){

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(nodesPrecedingNodesToMove(), startIndice)
      else nodesPrecedingNodesToMove()

    cleanRecordedMoves()

    val relevantNeighborsNow = relevantNeighbors()

    for (beforeMovedPoint <- iterationSchemeOnZone
         if vrp.isRouted(beforeMovedPoint)) {

      val movedPoint = vrp.next(beforeMovedPoint).value

      this.beforeMovedPoint = beforeMovedPoint

      for (
        insertionPoint <- relevantNeighborsNow(movedPoint)
        if (vrp.isRouted(insertionPoint)
          && (!vrp.isADepot(vrp.next(insertionPoint).value))
          && beforeMovedPoint != insertionPoint
          && movedPoint != insertionPoint
          && beforeMovedPoint != vrp.next(insertionPoint).value)
          && (!vrp.isADepot(movedPoint)
          || vrp.onTheSameRoute(movedPoint, insertionPoint))
      ) {

        this.insertionPoint = insertionPoint
        encode(beforeMovedPoint, insertionPoint)

        if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjOnEncodedMove())) {
          startIndice = beforeMovedPoint + 1
          return
        }
      }
    }
  }

  var beforeMovedPoint:Int = 0
  var insertionPoint:Int = 0

  override def instantiateCurrentMove(newObj: Int) =
    SwapMove(beforeMovedPoint, insertionPoint, newObj, this, neighborhoodNameToString)

  def encode(fstPred: Int, sndPred: Int) {
    val fstSeg = cutNodeAfter(fstPred)
    val sndSeg = cutNodeAfter(sndPred)
    insert(fstSeg, sndPred)
    insert(sndSeg, fstPred)
  }

  override def reset(): Unit = {
    startIndice = 0
  }
}

/**
 * Models a swap move of a given VRP problem.
 * @param fstPred the predecessor of the first point that will be swapped.
 * @param sndPred the predecessor of the second point that will be swapped.
 * @param objAfter the objective value if we performed this swap move.
 * @param neighborhood the originating neighborhood.
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
case class SwapMove(fstPred: Int,
                    sndPred: Int,
                    override val objAfter: Int,
                    override val neighborhood:Swap,
                    override val neighborhoodName:String = null) extends VRPMove(objAfter, neighborhood, neighborhoodName){

  override def impactedPoints: List[Int] = List(fstPred,sndPred)

  def encodeMove() {
    neighborhood.encode(fstPred, sndPred)
  }

  override def toString: String = (
    "Swap(first point predecessor = " + fstPred
      + ", second point predecessor = " + sndPred + " )")
}
