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
 *     Refactored (in respect with the new architecture) by Yoann Guyot.
 * ****************************************************************************
 */

package oscar.cbls.routing.seq.neighborhood

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


import oscar.cbls.algo.search.HotRestart
import oscar.cbls.routing.seq.model.VRP
import oscar.cbls.search.core.EasyNeighborhood
import oscar.cbls.search.move.Move

/**
 * Removes a point of route.
 * The search complexity is O(n).
 * @param relevantPointsToRemove: the predecessors ofthe points that we will try to remove
 * @param vrp the routing problem
 * @param neighborhoodName the name of the neighborhood, for verbosities
 * @param best true for the best move, false for the first move
 * @param hotRestart true if hotRestart is needed, false otherwise
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class RemovePoint(relevantPointsToRemove:()=>Iterable[Int],
                       vrp: VRP,
                       neighborhoodName:String = "RemovePoint",
                       best:Boolean = false,
                       hotRestart:Boolean = true)
  extends EasyNeighborhood[RemovePointMove](best,neighborhoodName){

  //the indice to start with for the exploration
  var startIndice: Int = 0

  var pointToRemove:Int = 0;
  var positionOfPointToRemove:Int = 0;

  val v = vrp.v
  val seq = vrp.routes


  override def exploreNeighborhood(): Unit = {

    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(relevantPointsToRemove(), startIndice)
      else relevantPointsToRemove()

    val it = iterationSchemeOnZone.iterator
    while (it.hasNext) {
      pointToRemove = it.next()

      seq.value.positionOfAnyOccurrence(pointToRemove) match{
        case None => ;
        case Some(p) =>
          positionOfPointToRemove = p
          doMove(positionOfPointToRemove)
          if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjAndRollBack())) {
            seq.releaseTopCheckpoint()
            startIndice = pointToRemove + 1
            return
          }
      }
    }
    seq.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Int) =
    RemovePointMove(positionOfPointToRemove, pointToRemove, vrp, newObj, this, neighborhoodNameToString)

  def doMove(positionOfPointToRemove: Int) {
    seq.remove(positionOfPointToRemove)
  }

  //this resets the internal state of the Neighborhood
  override def reset(){startIndice = 0}
}

/**
 * Models a remove-point operator of a given VRP problem.
 * @param positionOfPointToRemove the predecessor of the point that will be removed.
 * @param objAfter the objective value if we performed this remove-point operator.
 * @param neighborhood the originating neighborhood
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class RemovePointMove(positionOfPointToRemove: Int,
                           pointToRemove:Int,
                           vrp:VRP,
                           override val objAfter:Int,
                           override val neighborhood:RemovePoint,
                           override val neighborhoodName:String = null)
  extends VRPSMove(objAfter, neighborhood, neighborhoodName, vrp){

  override def impactedPoints: List[Int] = List(pointToRemove)

  override def commit() {
    neighborhood.doMove(positionOfPointToRemove)
  }
  override def toString: String = "RemovePoint(point:" + pointToRemove + objToString + ")"
}
