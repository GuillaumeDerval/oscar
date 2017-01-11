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
  *
  *     Refactored with respect to the new architecture by Yoann Guyot
  * ****************************************************************************
  */


package oscar.cbls.business.routing.neighborhood

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


import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.search.HotRestart
import oscar.cbls.business.routing.model.PDP
import oscar.cbls.core.search.EasyNeighborhood

/**
  * Removes three edges of routes, and rebuilds routes from the segments.
  * Finds 3 candidate points for a 3-opt move, and then
  * chooses on-the-fly between simple 3-opt move and reverse 3-opt move.
  *
  * Info : it also could be saw as the move of a route's segment to another place.
  * The search complexity is O(n³).
 *
  * @author renaud.delandtsheer@cetic.be
  * @author yoann.guyot@cetic.be
  * @author Florent Ghilain (UMONS)
  */
case class PickupDeliveryThreeOpt(potentialInsertionPoints:()=>Iterable[Int], //must be routed
                                  relevantNeighbors:()=>Int=>Iterable[Int], //must be routed
                                  pdp: PDP,
                                  neighborhoodName:String = "PickupDeliveryThreeOpt",
                                  best:Boolean = false,
                                  hotRestart:Boolean = true,
                                  skipOnePointMove:Boolean = false,
                                  breakSymmetry:Boolean = true,
                                  tryFlip:Boolean = true)
  extends EasyNeighborhood[PickupDeliveryThreeOptMove](best,neighborhoodName) {

  //the indice to start with for the exploration
  var startIndice: Int = 0

  val v = pdp.v
  val seq = pdp.routes

  def exploreNeighborhood(): Unit = {
    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(potentialInsertionPoints(), startIndice)
      else potentialInsertionPoints()

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }


    val nodeToVehicle = pdp.getVehicleOfAllNodes

    val positionOfAllNode = pdp.getRoutePositionOfAllNode

    for (insertionPoint <- iterationSchemeOnZone){
      seqValue.positionOfAnyOccurrence(insertionPoint) match{
        case None => //not routed?!
        case Some(insertionPosition) =>

          val completeSegments = Array.tabulate(v)(v => pdp.getCompleteSegments(v))

          val vehicleForInsertion = nodeToVehicle(insertionPoint)
          for( i <- 0 until v){
            val relevantNeighborsNow = relevantNeighbors()(insertionPoint)

            for ((segmentStart,segmentEnd) <- completeSegments(i)) {
              if(relevantNeighborsNow.toList.contains(segmentStart)){
                val segmentStartPosition = seqValue.positionOfAnyOccurrence(segmentStart).head
                val segmentEndPosition = seqValue.positionOfAnyOccurrence(segmentEnd).head

                if (nodeToVehicle(segmentStart) != vehicleForInsertion || (insertionPosition < segmentStartPosition && segmentEndPosition < insertionPosition)) {

                  segmentStartPositionForInstantiation = segmentStartPosition
                  segmentEndPositionForInstantiation = segmentEndPosition
                  insertionPointPositionForInstantiation = insertionPosition
                  insertionPointForInstantiation = insertionPoint

                  //skip this if same vehicle, no flip, and to the left

                  if (!breakSymmetry || vehicleForInsertion != nodeToVehicle(segmentStart) || insertionPosition > segmentStartPosition) {
                    //try move no flip
                    flipForInstantiation = false
                    doMove(insertionPosition, segmentStartPosition, segmentEndPosition, false)

                    if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjAndRollBack())) {
                      seq.releaseTopCheckpoint()
                      startIndice = insertionPoint + 1
                      return
                    }
                  }

                  if (tryFlip) {
                    //try move with flip
                    flipForInstantiation = true
                    doMove(insertionPosition, segmentStartPosition, segmentEndPosition, true)

                    if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjAndRollBack())) {
                      seq.releaseTopCheckpoint()
                      startIndice = insertionPoint + 1
                      segmentStartPositionForInstantiation = -1
                      return
                    }
                  }
                }
              }
            }
          }
      }
    }
    seq.releaseTopCheckpoint()
    segmentStartPositionForInstantiation = -1
  }

  var segmentStartPositionForInstantiation:Int = -1
  var segmentEndPositionForInstantiation:Int = -1
  var insertionPointPositionForInstantiation:Int = -1
  var insertionPointForInstantiation:Int = -1
  var flipForInstantiation:Boolean = false

  override def instantiateCurrentMove(newObj: Int) =
    PickupDeliveryThreeOptMove(segmentStartPositionForInstantiation,
      segmentEndPositionForInstantiation,
      insertionPointPositionForInstantiation,
      insertionPointForInstantiation,
      flipForInstantiation,
      newObj,
      this,
      neighborhoodName)

  //this resets the internal state of the Neighborhood
  override def reset(){
    startIndice = 0
  }

  def doMove(insertionPosition: Int, segmentStartPosition: Int, segmentEndPosition: Int, flip: Boolean) {
    seq.move(segmentStartPosition,segmentEndPosition,insertionPosition,flip)
  }
}


case class PickupDeliveryThreeOptMove(segmentStartPosition:Int,
                        segmentEndPosition:Int,
                        insertionPointPosition: Int,
                        insertionPoint:Int,
                        flipSegment: Boolean,
                        override val objAfter: Int,
                        override val neighborhood:PickupDeliveryThreeOpt,
                        override val neighborhoodName:String = "PickupDeliveryThreeOptMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName,neighborhood.pdp){

  override def impactedPoints: Iterable[Int] = QList(insertionPoint,neighborhood.pdp.routes.value.valuesBetweenPositionsQList(segmentStartPosition,segmentEndPosition))

  // overriding methods
  override def commit() {
    neighborhood.doMove(insertionPointPosition, segmentStartPosition, segmentEndPosition, flipSegment)
  }

  override def toString: String =
    neighborhoodNameToString + "TreeOpt(segmentStartPosition:" + segmentStartPosition +
      " segmentEndPosition:" + segmentEndPosition +
      " insertionPointPosition:" + insertionPointPosition +
      " insertionPoint:" + insertionPoint +
      (if(flipSegment) " flip" else " noFlip") + objToString + ")"
}
