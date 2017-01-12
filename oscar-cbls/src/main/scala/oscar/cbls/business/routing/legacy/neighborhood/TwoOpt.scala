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

package oscar.cbls.business.routing.legacy.neighborhood

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.business.routing.legacy.model._

import scala.collection.immutable.SortedSet

/**
 * Removes two edges of routes, and rebuilds routes from the segments.
 * (with one reverse required)
 *
 * The search complexity is O(n²).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
case class TwoOpt(predecesorOfFirstMovedPoint:()=>Iterable[Int],
                  relevantNeighbors:()=>Int=>Iterable[Int],
                  override val vrp: VRP with PositionInRouteAndRouteNr,
                  neighborhoodName:String = "TwoOpt",
                  best:Boolean = false,
                  hotRestart:Boolean = true) extends EasyRoutingNeighborhood[TwoOptMove](best,vrp,neighborhoodName) {

  //the indice to start with for the exploration
  var startIndice: Int = 0

  /**
   * Removes two edges of a route and flips the obtained segment before
   * reconnecting it.
   * The search complexity is O(n²).
   */
  override def exploreNeighborhood(): Unit = {

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(predecesorOfFirstMovedPoint(), startIndice)
      else predecesorOfFirstMovedPoint()

    cleanRecordedMoves()
    val relevantNeighborsNow = relevantNeighbors()

    for (fstPred <- iterationSchemeOnZone) {
      assert(vrp.isRouted(fstPred),
        "The search zone should be restricted to routed.")

      val nodesOnTheSameRouteAsFstPred = SortedSet.empty[Int] ++ vrp.getRouteOfVehicle(vrp.routeNr(fstPred).value)

      for (
        sndPred <- relevantNeighborsNow(fstPred) if (vrp.isRouted(sndPred)
        && sndPred != fstPred
        && fstPred != vrp.next(sndPred).newValue
        && nodesOnTheSameRouteAsFstPred.contains(sndPred))
      ) {

        this.fstPred = fstPred
        this.sndPred = sndPred

        encode(fstPred, sndPred)

        if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjOnEncodedMove())) {
          startIndice = fstPred + 1
          return
        }
      }
    }
  }

  var fstPred:Int = 0
  var sndPred:Int = 0

  override def instantiateCurrentMove(newObj: Int) =
    TwoOptMove(fstPred, sndPred, newObj, this, neighborhoodName)

  def encode(fstPred:Int, sndPred:Int) {
    val seg = cut(fstPred, sndPred)
    val rev_seg = reverse(seg)
    insert(rev_seg, fstPred)
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

/**
 * Models a two-opt-move operator of a given VRP problem.
 * @param fstPred the start of first edge that we remove.
 * @param sndPred the start of second edge that we remove.
 * @param objAfter the objective value if we performed this two-opt-move operator.
 * @param neighborhood the originating neighborhood
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
case class TwoOptMove(
  fstPred: Int,
  sndPred: Int,
  override val objAfter: Int,
  override val neighborhood:TwoOpt,
  override val neighborhoodName:String = "TwoOptMove")
  extends VRPMove(objAfter, neighborhood, neighborhoodName){

  override def impactedPoints: List[Int] = List(fstPred,sndPred)

  override def encodeMove() {
    neighborhood.encode(fstPred, sndPred)
  }

  override def toString: String = (neighborhoodNameToString + "TwoOpt(beforeSegStart:"
    + fstPred
    + "; end:" + sndPred + objToString + ")")
}
