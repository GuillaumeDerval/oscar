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

import oscar.cbls.algo.search.{Pairs, HotRestart}
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search.{First, LoopBehavior, EasyNeighborhoodMultiLevel, EasyNeighborhood}

import scala.collection.immutable.SortedSet

/**
 * exchanges segments of different vehicles (not on the same vehicle!)
 *
 * @param vrp the routing problem
 * @param relevantNeighbors given the start and end of the first segment, which are the relevant neighbors for the other segment? (will be filtered for vehicle by the neighborhood)
 * @param vehicles the set of vehicles to consider
 * @param neighborhoodName the name of the neighborhood, used for verbosities
 * @param segmentsToExchangeGroupedByVehicles Lists of tuples that represent the starting node position and
  *                                            the ending node position of the segment grouped by vehicles
 * @param hotRestart
 * @param tryFlip if false, will not flip any segment (maybe you do not want flipping if using time windows?)
 */

case class SegmentExchange(val vrp: VRP,
                           relevantNeighbors:()=>Int=>Iterable[Int], //must be routed
                           vehicles:() => Iterable[Int],
                           segmentsToExchangeGroupedByVehicles: Option[Map[Int,List[(Int,Int)]]] = None,
                           neighborhoodName:String = "SegmentExchange",
                           hotRestart:Boolean = true,

                           selectFirstVehicleBehavior:LoopBehavior = First(),
                           selectFirstNodeOfFirstSegmentBehavior:LoopBehavior = First(),
                           selectSecondNodeOfFirstSegmentBehavior:LoopBehavior = First(),
                           selectFirstNodeOfSecondSegmentBehavior:LoopBehavior = First(),
                           selectSecondNodeOfSecondSegmentBehavior:LoopBehavior = First(),

                           tryFlip:Boolean = true)
  extends EasyNeighborhoodMultiLevel[SegmentExchangeMove](neighborhoodName) {

  require(segmentsToExchangeGroupedByVehicles.isEmpty || segmentsToExchangeGroupedByVehicles.get.size == vrp.v,
    "If defined, segmentsToExchangeGroupedByVehicles must content segments for all vehicle therefore it must have a size of " + vrp.v)

  var firstSegmentStartPosition:Int = -1
  var firstSegmentEndPosition:Int = -1
  var flipFirstSegment:Boolean = false
  var secondSegmentStartPosition: Int = -1
  var secondSegmentEndPosition: Int = -1
  var flipSecondSegment:Boolean = false
  var startVehicle = 0

  val v = vrp.v
  val seq = vrp.routes

  val n = vrp.n

  override def exploreNeighborhood() {

    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    val relevantNeighborsNow = relevantNeighbors()

    val nodeToRoute:Array[Int] = vrp.getVehicleOfAllNodes

    val listOfVehiclesToIterateOn = (if (hotRestart) HotRestart(vehicles(), startVehicle) else vehicles()).toList
    var allVehiclesToIterateOn = SortedSet.empty[Int] ++ listOfVehiclesToIterateOn

    val (listOfVehiclesToIterateOnIterable,notifyFound1) = selectFirstVehicleBehavior.toIterable(listOfVehiclesToIterateOn)
    var firstVehicle = -1

    for(firstVehicleTmp <- listOfVehiclesToIterateOnIterable){

      firstVehicle = firstVehicleTmp

      allVehiclesToIterateOn = allVehiclesToIterateOn - firstVehicle

      val routeOfVehicle1: List[Int] = {
        if(segmentsToExchangeGroupedByVehicles.isDefined) {
          val segments = segmentsToExchangeGroupedByVehicles.get(firstVehicle)
          val segmentsStart = segments.map(_._1)
          segmentsStart
        } else {
          vrp.getRouteOfVehicle(firstVehicle)
        }
      }

      val routeWithRelevantNeighborsTheirVehicleAndPositionGroupedByVehicles:List[(Int,Int,Map[Int,Iterable[(Int,Int,Int)]])] = routeOfVehicle1.map(node =>
        (node, seqValue.positionOfAnyOccurrence(node).head, relevantNeighborsNow(node)
          .map(node => (node,if(node >=v && nodeToRoute(node)!=n) nodeToRoute(node) else -1))
          .filter({case (node,routeNr) => node >= v && allVehiclesToIterateOn.contains(routeNr)})
          .map(nodeAndRoute => (nodeAndRoute._1,nodeAndRoute._2,seqValue.positionOfAnyOccurrence(nodeAndRoute._1).head))
          .groupBy(nodeAndRoute => nodeAndRoute._2))
      )

      val (routeWithRelevantNeighborsTheirVehicleAndPositionGroupedByVehiclesIterableAndTail,notifyFound2) =
        selectFirstNodeOfFirstSegmentBehavior.toIterable(Pairs.makeAllHeadAndTails(routeWithRelevantNeighborsTheirVehicleAndPositionGroupedByVehicles))
      for(((firstNode, positionOfFirstNode, firstNodeVehicleToNodeRoutePosition),candidateForAfterEndOfFirstSegment)
          <- routeWithRelevantNeighborsTheirVehicleAndPositionGroupedByVehiclesIterableAndTail){

        val (candidateForAfterEndOfFirstSegmentIterable,notifyFound3) = selectSecondNodeOfFirstSegmentBehavior.toIterable(candidateForAfterEndOfFirstSegment)

        for ((secondNode, positionOfSecondNode, secondNodeVehicleToNodeRoutePosition) <- candidateForAfterEndOfFirstSegmentIterable){

          //we define the first segment

          val isReversedFromFirstSecondNodesFirstSegment =
            if (positionOfFirstNode < positionOfSecondNode) {
              firstSegmentStartPosition = positionOfFirstNode + 1
              firstSegmentEndPosition = positionOfSecondNode - 1
              false
            } else {
              firstSegmentStartPosition = positionOfSecondNode + 1
              firstSegmentEndPosition = positionOfFirstNode - 1
              true
            }

          //we check that the first segment is not empty
          if(firstSegmentStartPosition <= firstSegmentEndPosition) {
            //now we search for nodes in other vehicles
            val otherVehicles : Iterable[Int] = firstNodeVehicleToNodeRoutePosition.keys.filter((v : Int) => secondNodeVehicleToNodeRoutePosition.isDefinedAt(v))
            for (otherVehicle <- otherVehicles) {

              val (relevantNeighborsForFirstNodeNodeVPos:Iterable[(Int, Int, Int)],notifyFound4) =
                selectFirstNodeOfSecondSegmentBehavior.toIterable(firstNodeVehicleToNodeRoutePosition(otherVehicle))

              val (relevantNeighborsForSecondNodeNodeVPos:Iterable[(Int, Int, Int)],notifyFound5) =
                selectSecondNodeOfSecondSegmentBehavior.toIterable(secondNodeVehicleToNodeRoutePosition(otherVehicle))

              //TODO: double loop and some post-filtering is naive, some pre-filtering could be done before, eg based on a sort of the relevant neighbors by position
              for ((relevantFirstNode, _, relevantFirstPos) <- relevantNeighborsForFirstNodeNodeVPos) {
                for ((relevantSecondNode, _, relevantSecondPos) <- relevantNeighborsForSecondNodeNodeVPos) {


                  val isReversedFromFirstSecondNodesSecondSegment =
                    if (relevantFirstPos < relevantSecondPos) {
                      secondSegmentStartPosition = relevantFirstPos + 1
                      secondSegmentEndPosition = relevantSecondPos - 1
                      false
                    } else {
                      secondSegmentStartPosition = relevantSecondPos + 1
                      secondSegmentEndPosition = relevantFirstPos - 1
                      true
                    }

                  if(secondSegmentStartPosition <= secondSegmentEndPosition) {

                    flipSecondSegment = isReversedFromFirstSecondNodesSecondSegment != isReversedFromFirstSecondNodesFirstSegment
                    flipFirstSegment = flipSecondSegment

                    if(tryFlip || !flipSecondSegment) {
                      doMove(firstSegmentStartPosition, firstSegmentEndPosition, flipFirstSegment,
                        secondSegmentStartPosition, secondSegmentEndPosition, flipSecondSegment)

                      if (evaluateCurrentMoveObjTrueIfSomethingFound(evalObjAndRollBack())) {
                        notifyFound1()
                        notifyFound2()
                        notifyFound3()
                        notifyFound4()
                        notifyFound5()
                      }
                    }
                  }// end if second segment nonempty
                }
              }
            } //end for otherVehicle
          }//end if  first segment not empty
        }//end loop on second node first segment
      }//end loop on first node first segment

    }//end loop on vehicles
    seq.releaseTopCheckpoint()
    startVehicle = firstVehicle + 1
  } //end def

  override def instantiateCurrentMove(newObj: Int): SegmentExchangeMove = {
    SegmentExchangeMove(
      firstSegmentStartPosition, firstSegmentEndPosition,flipFirstSegment,
      secondSegmentStartPosition, secondSegmentEndPosition, flipSecondSegment,
      newObj, this, neighborhoodName)
  }

  def doMove(firstSegmentStartPosition:Int, firstSegmentEndPosition:Int, flipFirstSegment:Boolean,
             secondSegmentStartPosition: Int, secondSegmentEndPosition: Int, flipSecondSegment:Boolean){
    seq.swapSegments(firstSegmentStartPosition,
      firstSegmentEndPosition,
      flipFirstSegment,
      secondSegmentStartPosition,
      secondSegmentEndPosition,
      flipSecondSegment)
  }
}


case class SegmentExchangeMove(firstSegmentStartPosition:Int,
                               firstSegmentEndPosition:Int,
                               flipFirstSegment:Boolean,
                               secondSegmentStartPosition: Int,
                               secondSegmentEndPosition: Int,
                               flipSecondSegment:Boolean,
                               override val objAfter: Int,override val neighborhood:SegmentExchange,
                               override val neighborhoodName:String = "SegmentExchangeMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName,neighborhood.vrp){

  override def impactedPoints: Iterable[Int] =
    neighborhood.vrp.routes.value.valuesBetweenPositionsQList(firstSegmentStartPosition,firstSegmentEndPosition) ++
      neighborhood.vrp.routes.value.valuesBetweenPositionsQList(secondSegmentStartPosition,secondSegmentEndPosition)

  override def commit() {
    neighborhood.doMove(
      firstSegmentStartPosition,firstSegmentEndPosition, flipFirstSegment,
      secondSegmentStartPosition, secondSegmentEndPosition, flipSecondSegment)
  }

  override def toString: String = {
    neighborhoodNameToString + "SegmentExchange(firstSegmentStartPosition:" + firstSegmentStartPosition + " firstSegmentEndPosition:" + firstSegmentEndPosition + " flipFirstSegment:" + flipFirstSegment +
      " secondSegmentStartPosition:" + secondSegmentStartPosition + " secondSegmentEndPosition:" + secondSegmentEndPosition + " flipSecondSegment:" + flipSecondSegment + objToString + ")"
  }
}



/*
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
    def routePosition(node: Int) = pdp.routes.value.positionOfAnyOccurrence(node)

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    val completeSegments:List[List[(Int,Int)]] = List.tabulate(pdp.v)(v => pdp.getCompleteSegments(v))

    if(!hotRestart)startVehicle = 0

    val relevantNeighborsNow = completeSegments.flatten.map(x => x._1 -> relevantNeighbors()(x._2)).toMap

    for(firstVehicle <- startVehicle until pdp.v - 1){
      for(firstSegment <- completeSegments(firstVehicle)){
        firstSegmentStartPosition = routePosition(firstSegment._1).get
        firstSegmentEndPosition = routePosition(firstSegment._2).get
        val prevOfFirstSegment = pdp.prev(firstSegment._1).value
        val nextOfFirstSegment = pdp.next(firstSegment._2).value
        val durationOfFirstSegment = pdp.leaveTimes(firstSegment._2).value - Math.max(pdp.arrivalTimes(firstSegment._1).value,pdp.earlylines(firstSegment._1))
        for(secondVehicle <- firstVehicle+1 until pdp.v){
          for(secondSegment <- completeSegments(secondVehicle) if relevantNeighborsNow(firstSegment._1).toList.contains(pdp.prev(secondSegment._1).value)){
            secondSegmentStartPosition = routePosition(secondSegment._1).get
            secondSegmentEndPosition = routePosition(secondSegment._2).get
            val prevOfSecondSegment = pdp.prev(secondSegment._1).value
            val nextOfSecondSegment = pdp.next(secondSegment._2).value
            val durationOfSecondSegment = pdp.leaveTimes(secondSegment._2).value - Math.max(pdp.arrivalTimes(secondSegment._1).value,pdp.earlylines(secondSegment._1))

            if(pdp.leaveTimes(prevOfFirstSegment).value < pdp.deadlines(secondSegment._1) &&
              pdp.leaveTimes(prevOfSecondSegment).value < pdp.deadlines(firstSegment._1) &&
              pdp.deadlines(nextOfFirstSegment) > (pdp.leaveTimes(prevOfFirstSegment).value + durationOfSecondSegment) &&
              pdp.deadlines(nextOfSecondSegment) > (pdp.leaveTimes(prevOfSecondSegment).value + durationOfFirstSegment))
            {
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
*/