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

package oscar.cbls.routing.seq.neighborhood

import oscar.cbls.invariants.lib.routing.RoutingConventionMethods
import oscar.cbls.routing.seq.model.VRP
import oscar.cbls.search.algo.HotRestart
import oscar.cbls.search.core.EasyNeighborhood


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
case class TwoOpt(segmentStartValues:()=>Iterable[Int],
                  closeNeighbors:()=>Int=>Iterable[Int],
                  vrp: VRP,
                  neighborhoodName:String = "TwoOpt",
                  best:Boolean = false,
                  hotRestart:Boolean = true) extends EasyNeighborhood[TwoOptMove](best,neighborhoodName) {

  val v = vrp.v
  val seq = vrp.seq
  //the indice to start with for the exploration
  var startIndice: Int = 0

  /**
   * Removes two edges of a route and flips the obtained segment before
   * reconnecting it.
   * The search complexity is O(n²).
   */
  override def exploreNeighborhood(): Unit = {

    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(segmentStartValues(), startIndice)
      else segmentStartValues()

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToCurrentCheckpoint(seqValue)
      a
    }

    val relevantNeighborsNow = closeNeighbors()

    val nodesOfVehicle:Array[SortedSet[Int]] = Array.fill(v)(null)
    for (segmentStartValue <- iterationSchemeOnZone if segmentStartValue >= v) {
      assert(vrp.isRouted(segmentStartValue),
        "The search zone should be restricted to routed.")

      val segmentStartPositionExplorer =seqValue.explorerAtAnyOccurrence(segmentStartValue).head
      val segmentStartPosition = segmentStartPositionExplorer.position
      val predecessorOfSegmentStartValue = segmentStartPositionExplorer.prev.head.value

      val vehicleReachingSegmentStart = RoutingConventionMethods.searchVehicleReachingPosition(segmentStartPosition,seqValue,v)
      if(nodesOfVehicle(vehicleReachingSegmentStart) == null){
        nodesOfVehicle(vehicleReachingSegmentStart) = vrp.getNodesOfVehicle(vehicleReachingSegmentStart)
      }

      val nodesOnTheSameRouteAsSegmentStart = nodesOfVehicle(vehicleReachingSegmentStart)

      for (
        segmentEndValue <- relevantNeighborsNow(predecessorOfSegmentStartValue)
        if (segmentEndValue >= v
          && nodesOnTheSameRouteAsSegmentStart.contains(segmentEndValue))
      ) {

        val segmentEndPosition = seqValue.positionOfAnyOccurrence(segmentEndValue).head

        if(segmentEndPosition > segmentStartPosition) {

          segmentStartPositionForInstantiate = segmentStartPosition
          segmentEndPositionForInstantiate = segmentEndPosition

          doMove(segmentStartPosition, segmentEndPosition)

          if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjAndRollBack())) {
            seq.releaseCurrentCheckpointAtCheckpoint()
            startIndice = segmentStartValue + 1
            return
          }
        }
      }
    }
    seq.releaseCurrentCheckpointAtCheckpoint()
  }

  var segmentStartPositionForInstantiate:Int = 0
  var segmentEndPositionForInstantiate:Int = 0

  override def instantiateCurrentMove(newObj: Int) =
    TwoOptMove(segmentStartPositionForInstantiate, segmentEndPositionForInstantiate, newObj, this, neighborhoodName)

  def doMove(fromPositionIncluded:Int,toPositionIncluded:Int) {
    seq.flip(fromPositionIncluded,toPositionIncluded)
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

/**
 * Models a two-opt-move operator of a given VRP problem.
 * @param objAfter the objective value if we performed this two-opt-move operator.
 * @param neighborhood the originating neighborhood
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
case class TwoOptMove(segmentStartPosition:Int,
                       segmentEndPosition:Int,
                       override val objAfter: Int,
                       override val neighborhood:TwoOpt,
                       override val neighborhoodName:String = "TwoOptMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName, neighborhood.vrp){

  override def impactedPoints: Iterable[Int] = neighborhood.vrp.seq.value.valuesBetweenPositions(segmentStartPosition,segmentEndPosition)

  override def commit() {
    neighborhood.doMove(segmentStartPosition, segmentEndPosition)
  }

  override def toString: String = (neighborhoodNameToString + "TwoOpt(segmentStartPosition:"
    + segmentStartPosition
    + "; segmentEndPosition:" + segmentEndPosition + objToString + ")")
}
