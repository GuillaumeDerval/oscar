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


import oscar.cbls.business.routing.model.PDP
import oscar.cbls.business.routing.neighborhood.VRPSMove
import oscar.cbls.core.search.EasyNeighborhood

/**
 * This is a version of segmentExchange specialized for the pickup & delivery problems.
 * It uses a method called computeCompleteSegments that returns the list of segments the neighborhood will use during his search.
 * So no relevant neighbors or whatsoever is needed.
 * Due to the precedence constraint, it isn't allowed to flip a segment.
 *
 * @param pdp The PDP object specific for pickup & delivery problems
 * @param neighborhoodName the name of the neighborhood, used for verbosities
 * @param hotRestart true if you doesn't wan't to test all the route each time the neighborhood is called
 * @param best true if you want the best move false if you want the first acceptable move
 */
case class PickupDeliverySegmentExchange(pdp: PDP,
                                         relevantNeighbors:()=>Int=>Iterable[Int], //must be routed
                                         neighborhoodName:String = "PickupDeliverySegmentExchange",
                                         hotRestart:Boolean = true,
                                         best:Boolean = false)
  extends EasyNeighborhood[PickupDeliverySegmentExchangeMove](best,neighborhoodName){

  var firstSegmentStartPosition:Int = -1
  var firstSegmentEndPosition:Int = -1
  var secondSegmentStartPosition: Int = -1
  var secondSegmentEndPosition: Int = -1
  var startVehicle = 0

  val n = pdp.n
  val v = pdp.v

  val seq = pdp.routes


  override def exploreNeighborhood(): Unit = {
    val seqValue = seq.defineCurrentValueAsCheckpoint(true)
    def positionsOfValue(value:Int) = pdp.routes.newValue.positionOfAnyOccurrence(value)

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    val completeSegments:List[List[(Int,Int)]] = List.tabulate(pdp.v)(v => pdp.getCompleteSegments(v))

    if(!hotRestart)startVehicle = 0

    val relevantNeighborsNow = relevantNeighbors()
    for(firstVehicle <- startVehicle until pdp.v - 1){
      for(firstSegment <- completeSegments(firstVehicle)){
        firstSegmentStartPosition = positionsOfValue(firstSegment._1).get
        firstSegmentEndPosition = positionsOfValue(firstSegment._2).get
        for(secondVehicle <- firstVehicle+1 until pdp.v){
          for(secondSegment <- completeSegments(secondVehicle) if relevantNeighborsNow(firstSegment._1).toList.contains(pdp.prev(secondSegment._1).value)){
            secondSegmentStartPosition = positionsOfValue(secondSegment._1).get
            secondSegmentEndPosition = positionsOfValue(secondSegment._2).get

            val (firstStartEarly,firstStartDead) = (if(pdp.timeWindows(firstSegment._1)._1 == -1)Int.MaxValue else pdp.timeWindows(firstSegment._1)._1,
              if(pdp.timeWindows(firstSegment._1)._2 == -1)Int.MaxValue else pdp.timeWindows(firstSegment._1)._2)
            val (firstEndEarly,firstEndDead) = (if(pdp.timeWindows(firstSegment._2)._1 == -1)Int.MaxValue else pdp.timeWindows(firstSegment._2)._1,
              if(pdp.timeWindows(firstSegment._2)._2 == -1)Int.MaxValue else pdp.timeWindows(firstSegment._2)._2)
            val (secondStartEarly,secondStartDead) = (if(pdp.timeWindows(secondSegment._1)._1 == -1)Int.MaxValue else pdp.timeWindows(secondSegment._1)._1,
              if(pdp.timeWindows(secondSegment._1)._2 == -1)Int.MaxValue else pdp.timeWindows(secondSegment._1)._2)
            val (secondEndEarly,secondEndDead) = (if(pdp.timeWindows(secondSegment._2)._1 == -1)Int.MaxValue else pdp.timeWindows(secondSegment._2)._1,
              if(pdp.timeWindows(secondSegment._2)._2 == -1)Int.MaxValue else pdp.timeWindows(secondSegment._2)._2)

            if(secondStartEarly != Int.MaxValue && firstStartDead != Int.MaxValue && secondStartEarly > firstStartDead){}
            else if(secondStartDead != Int.MaxValue && firstStartEarly != Int.MaxValue && secondStartDead < firstStartEarly){}
            else if(secondEndEarly != Int.MaxValue && firstEndDead != Int.MaxValue && secondEndEarly > firstEndDead){}
            else if(secondEndDead != Int.MaxValue && firstEndEarly != Int.MaxValue && secondEndDead < firstEndEarly){}
            else if(secondStartEarly != Int.MaxValue && firstEndDead != Int.MaxValue && secondStartEarly > firstEndDead){}
            else if(secondEndDead != Int.MaxValue && firstStartEarly != Int.MaxValue && secondEndDead < firstStartEarly){}
            else {
              doMove(firstSegmentStartPosition, firstSegmentEndPosition, secondSegmentStartPosition, secondSegmentEndPosition)
              if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjAndRollBack())) {
                seq.releaseTopCheckpoint()
                startVehicle = firstVehicle + 1
                return
              }
            }
          }
        }
      }
    }
    seq.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Int): PickupDeliverySegmentExchangeMove = {
    PickupDeliverySegmentExchangeMove(
      firstSegmentStartPosition, firstSegmentEndPosition,
      secondSegmentStartPosition, secondSegmentEndPosition,
      newObj, this, neighborhoodName)
  }

  def doMove(firstSegmentStartPosition:Int, firstSegmentEndPosition:Int,
             secondSegmentStartPosition: Int, secondSegmentEndPosition: Int){
    seq.swapSegments(firstSegmentStartPosition,
      firstSegmentEndPosition,
      false,
      secondSegmentStartPosition,
      secondSegmentEndPosition,
      false)
  }
}

case class PickupDeliverySegmentExchangeMove(firstSegmentStartPosition:Int,
                                             firstSegmentEndPosition:Int,
                                             secondSegmentStartPosition: Int,
                                             secondSegmentEndPosition: Int,
                                             override val objAfter: Int,override val neighborhood:PickupDeliverySegmentExchange,
                                             override val neighborhoodName:String = "PickupDeliverySegmentExchangeMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName,neighborhood.pdp){

  override def impactedPoints: Iterable[Int] =
    neighborhood.pdp.routes.value.valuesBetweenPositionsQList(firstSegmentStartPosition,firstSegmentEndPosition) ++
      neighborhood.pdp.routes.value.valuesBetweenPositionsQList(secondSegmentStartPosition,secondSegmentEndPosition)

  override def commit() {
    neighborhood.doMove(
      firstSegmentStartPosition, firstSegmentEndPosition,
      secondSegmentStartPosition, secondSegmentEndPosition)
  }

  override def toString: String = {
    neighborhoodNameToString + "SegmentExchange(firstSegmentStartPosition:" + firstSegmentStartPosition + " firstSegmentEndPosition:" + firstSegmentEndPosition +
      " secondSegmentStartPosition:" + secondSegmentStartPosition + " secondSegmentEndPosition:" + secondSegmentEndPosition + objToString + ")"
  }
}
