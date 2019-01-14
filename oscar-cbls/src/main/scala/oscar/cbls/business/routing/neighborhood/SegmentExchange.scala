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

import oscar.cbls.algo.clique.Clique
import oscar.cbls.algo.search.{HotRestart, Pairs}
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search.{EasyNeighborhood, EasyNeighborhoodMultiLevel, First, LoopBehavior}

import scala.collection.immutable.{HashSet, SortedSet}

/**
 * exchanges segments of different vehicles (not on the same vehicle!)
 *
 * @param vrp the routing problem
 * @param relevantNeighbors given the start and end of the first segment, which are the relevant neighbors for the other segment? (will be filtered for vehicle by the neighborhood)
 * @param vehicles the set of vehicles to consider
 * @param neighborhoodName the name of the neighborhood, used for verbosities
 * @param hotRestart
 * @param tryFlip if false, will not flip any segment (maybe you do not want flipping if using time windows?)
 */
case class SegmentExchange(val vrp: VRP,
                           relevantNeighbors:()=>Long=>Iterable[Long], //must be routed
                           vehicles:() => Iterable[Long],
                           neighborhoodName:String = "SegmentExchange",
                           hotRestart:Boolean = true,

                           selectFirstVehicleBehavior:LoopBehavior = First(),
                           selectFirstNodeOfFirstSegmentBehavior:LoopBehavior = First(),
                           selectSecondNodeOfFirstSegmentBehavior:LoopBehavior = First(),
                           selectFirstNodeOfSecondSegmentBehavior:LoopBehavior = First(),
                           selectSecondNodeOfSecondSegmentBehavior:LoopBehavior = First(),

                           tryFlip:Boolean = true)
  extends EasyNeighborhoodMultiLevel[SegmentExchangeMove](neighborhoodName) {

  var firstSegmentStartPosition:Long = -1L
  var firstSegmentEndPosition:Long = -1L
  var flipFirstSegment:Boolean = false
  var secondSegmentStartPosition: Long = -1L
  var secondSegmentEndPosition: Long = -1L
  var flipSecondSegment:Boolean = false
  var startVehicle = 0L

  val v = vrp.v
  val seq = vrp.routes

  val n = vrp.n

  override def exploreNeighborhood(initialObj: Long){

    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    def evalObjAndRollBack() : Long = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    val relevantNeighborsNow = relevantNeighbors()

    val nodeToRoute:Array[Long] = vrp.vehicleOfNode.map(_.value)

    val listOfVehiclesToIterateOn = (if (hotRestart) HotRestart(vehicles(), startVehicle) else vehicles()).toList
    var allVehiclesToIterateOn = SortedSet.empty[Long] ++ listOfVehiclesToIterateOn

    val (listOfVehiclesToIterateOnIterable,notifyFound1) = selectFirstVehicleBehavior.toIterable(listOfVehiclesToIterateOn)
    var firstVehicle = -1L

    for(firstVehicleTmp <- listOfVehiclesToIterateOnIterable){

      firstVehicle = firstVehicleTmp

      allVehiclesToIterateOn = allVehiclesToIterateOn - firstVehicle

      val routeOfVehicle1: List[Long] = vrp.getRouteOfVehicle(firstVehicle)

      val routeWithRelevantNeighborsTheirVehicleAndPositionGroupedByVehicles:List[(Long,Long,Map[Long,Iterable[(Long,Long,Long)]])] = routeOfVehicle1.map(node =>
        (node, seqValue.positionOfAnyOccurrence(node).head, relevantNeighborsNow(node)
          .map(node => (node,if(node >=v && nodeToRoute(node)!=n) nodeToRoute(node) else -1L))
          .filter({case (nodeNr,routeNr) => nodeNr >= v && allVehiclesToIterateOn.contains(routeNr)})
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
              firstSegmentStartPosition = positionOfFirstNode + 1L
              firstSegmentEndPosition = positionOfSecondNode - 1L
              false
            } else {
              firstSegmentStartPosition = positionOfSecondNode + 1L
              firstSegmentEndPosition = positionOfFirstNode - 1L
              true
            }

          //we check that the first segment is not empty
          if(firstSegmentStartPosition <= firstSegmentEndPosition) {
            //now we search for nodes in other vehicles
            val otherVehicles : Iterable[Long] = firstNodeVehicleToNodeRoutePosition.keys.filter((v : Long) => secondNodeVehicleToNodeRoutePosition.isDefinedAt(v))
            for (otherVehicle <- otherVehicles) {

              val (relevantNeighborsForFirstNodeNodeVPos:Iterable[(Long, Long, Long)],notifyFound4) =
                selectFirstNodeOfSecondSegmentBehavior.toIterable(firstNodeVehicleToNodeRoutePosition(otherVehicle))

              val (relevantNeighborsForSecondNodeNodeVPos:Iterable[(Long, Long, Long)],notifyFound5) =
                selectSecondNodeOfSecondSegmentBehavior.toIterable(secondNodeVehicleToNodeRoutePosition(otherVehicle))

              //TODO: double loop and some post-filtering is naive, some pre-filtering could be done before, eg based on a sort of the relevant neighbors by position
              for ((relevantFirstNode, _, relevantFirstPos) <- relevantNeighborsForFirstNodeNodeVPos) {
                for ((relevantSecondNode, _, relevantSecondPos) <- relevantNeighborsForSecondNodeNodeVPos) {


                  val isReversedFromFirstSecondNodesSecondSegment =
                    if (relevantFirstPos < relevantSecondPos) {
                      secondSegmentStartPosition = relevantFirstPos + 1L
                      secondSegmentEndPosition = relevantSecondPos - 1L
                      false
                    } else {
                      secondSegmentStartPosition = relevantSecondPos + 1L
                      secondSegmentEndPosition = relevantFirstPos - 1L
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
    startVehicle = firstVehicle + 1L
  } //end def

  override def instantiateCurrentMove(newObj: Long): SegmentExchangeMove = {
    SegmentExchangeMove(
      firstSegmentStartPosition, firstSegmentEndPosition,flipFirstSegment,
      secondSegmentStartPosition, secondSegmentEndPosition, flipSecondSegment,
      newObj, this, neighborhoodName)
  }

  def doMove(firstSegmentStartPosition:Long, firstSegmentEndPosition:Long, flipFirstSegment:Boolean,
             secondSegmentStartPosition: Long, secondSegmentEndPosition: Long, flipSecondSegment:Boolean){
    seq.swapSegments(firstSegmentStartPosition,
      firstSegmentEndPosition,
      flipFirstSegment,
      secondSegmentStartPosition,
      secondSegmentEndPosition,
      flipSecondSegment)
  }
}



/**
  * This neighborhood try to exchange segments of route.
  * Unlike the SegmentExchange move, this one is based on a list of predefined segments.
  * In order to use it you have to build your own segments.
  *
  * This neighborhood can be useful and more powerful than the simple SegmentExchange one.
  * For example, when you use chains in your VRP, you'll have precedence constraints.
  * So a lot of movements won't be allowed because they will break the Precedence constraints.
  * (If the segment takes only a part of a chain, and move it to another route for instance)
  * In this case it's more interesting to specified the segments you want to exchange
  * (by using the method computeCompleteSegments() presents in ChainsHelper object)
  *
  * @param vrp The PDP object specific for pickup & delivery problems
  * @param segmentsToExchangeGroupedByVehicle The lists of segments grouped by routes
  * @param neighborhoodName the name of the neighborhood, used for verbosities
  * //@param tryFlip True if you want to try flipping the segments
  * @param hotRestart true if you doesn't wan't to test all the route each time the neighborhood is called
  */
case class SegmentExchangeOnSegments(vrp: VRP,
                                     segmentsToExchangeGroupedByVehicle: () => Map[Long, List[(Long,Long)]],
                                     relevantNeighbors:()=>Long=>Iterable[Long], //must be routed
                                     vehicles: () => Iterable[Long],
                                     neighborhoodName:String = "PickupDeliverySegmentExchange",
                                     hotRestart:Boolean = true,

                                     selectFirstVehicleBehavior:LoopBehavior = First(),
                                     selectSecondVehicleBehavior:LoopBehavior = First(),
                                     selectFirstSegmentBehavior:LoopBehavior = First(),
                                     selectSecondSegmentBehavior:LoopBehavior = First()

                                     //tryFlip: Boolean = false,
                                    )
  extends EasyNeighborhoodMultiLevel[SegmentExchangeOnSegmentsMove](neighborhoodName){

  var firstSegmentStartPosition:Long = -1L
  var firstSegmentEndPosition:Long = -1L
  var secondSegmentStartPosition: Long = -1L
  var secondSegmentEndPosition: Long = -1L
  var tryFlip: Boolean = false
  var startVehicle = 0L

  val n = vrp.n
  val v = vrp.v

  val seq = vrp.routes


  override def exploreNeighborhood(initialObj: Long): Unit = {
    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    val segmentsToExchangeGroupedByVehiclesNow = segmentsToExchangeGroupedByVehicle()
    val vehiclesNow = vehicles().toList.sorted
    val relevantNeighborsNow = relevantNeighbors()
    val isNodeInRelevantNeighborsOfNodes = Array.tabulate(n)(node => {
      val isNodesRelevant = Array.fill(n)(false)
      for (neighbor <- relevantNeighborsNow(node))
        isNodesRelevant(neighbor) = true
      isNodesRelevant
    })

    val routePositionOfNodes = vrp.getGlobalRoutePositionOfAllNode
    val prevNodeOfAllNodes = vrp.getGlobalPrevNodeOfAllNodes
    val nextNodeOfAllNodes = vrp.getGlobalNextNodeOfAllNodes

    def evalObjAndRollBack() : Long = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    def areSegmentExchangeable(segment1: (Long,Long), segment2: (Long,Long)): Boolean ={
      val (start1,end1) = segment1
      val (start2,end2) = segment2
      isNodeInRelevantNeighborsOfNodes(start1)(prevNodeOfAllNodes(start2)) &&
        isNodeInRelevantNeighborsOfNodes(start2)(prevNodeOfAllNodes(start1)) &&
        (nextNodeOfAllNodes(end1) < v || isNodeInRelevantNeighborsOfNodes(nextNodeOfAllNodes(end1))(end2)) &&
        (nextNodeOfAllNodes(end2) < v || isNodeInRelevantNeighborsOfNodes(nextNodeOfAllNodes(end2))(end1))
    }

    if(!hotRestart)startVehicle = 0L

    val (listOfVehicleForFirstVehicle,notifyFound1) = selectFirstVehicleBehavior.toIterable(vehiclesNow.drop(startVehicle))
    for(firstVehicle <- listOfVehicleForFirstVehicle){
      val (listOfVehicleForSecondVehicle,notifyFound2) = selectSecondVehicleBehavior.toIterable(vehiclesNow.dropWhile(_ <= firstVehicle))
      for(secondVehicle <- listOfVehicleForSecondVehicle){
        val (listOfFirstSegment,notifyFound3) = selectFirstSegmentBehavior.toIterable(segmentsToExchangeGroupedByVehiclesNow(firstVehicle))
        val (listOfSecondSegment,notifyFound4) = selectSecondSegmentBehavior.toIterable(segmentsToExchangeGroupedByVehiclesNow(secondVehicle))
        for((firstSegment, secondSegment) <- listOfFirstSegment.zip(listOfSecondSegment) if areSegmentExchangeable(firstSegment,secondSegment)){
          firstSegmentStartPosition = routePositionOfNodes(firstSegment._1)
          firstSegmentEndPosition = routePositionOfNodes(firstSegment._2)
          secondSegmentStartPosition = routePositionOfNodes(secondSegment._1)
          secondSegmentEndPosition = routePositionOfNodes(secondSegment._2)

          doMove(firstSegmentStartPosition, firstSegmentEndPosition, false, secondSegmentStartPosition, secondSegmentEndPosition, false)
          if (evaluateCurrentMoveObjTrueIfSomethingFound(evalObjAndRollBack())) {
            notifyFound1()
            notifyFound2()
            notifyFound3()
            notifyFound4()
          }
        }
      }
    }
    startVehicle += 1L
    seq.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Long): SegmentExchangeOnSegmentsMove = {
    SegmentExchangeOnSegmentsMove(
      firstSegmentStartPosition, firstSegmentEndPosition, false,
      secondSegmentStartPosition, secondSegmentEndPosition, false,
      newObj, this, neighborhoodName)
  }

  def doMove(firstSegmentStartPosition:Long, firstSegmentEndPosition:Long, flipFirstSegment: Boolean,
             secondSegmentStartPosition: Long, secondSegmentEndPosition: Long, flipSecondSegment: Boolean){
    seq.swapSegments(firstSegmentStartPosition,
      firstSegmentEndPosition,
      flipFirstSegment,
      secondSegmentStartPosition,
      secondSegmentEndPosition,
      flipSecondSegment)
  }
}

case class SegmentExchangeOnSegmentsMove(firstSegmentStartPosition:Long,
                               firstSegmentEndPosition:Long,
                               flipFirstSegment:Boolean,
                               secondSegmentStartPosition: Long,
                               secondSegmentEndPosition: Long,
                               flipSecondSegment:Boolean,
                               override val objAfter: Long,override val neighborhood:SegmentExchangeOnSegments,
                               override val neighborhoodName:String = "SegmentExchangeOnSegmentsMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName,neighborhood.vrp){

  override def impactedPoints: Iterable[Long] =
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

case class SegmentExchangeMove(firstSegmentStartPosition:Long,
                               firstSegmentEndPosition:Long,
                               flipFirstSegment:Boolean,
                               secondSegmentStartPosition: Long,
                               secondSegmentEndPosition: Long,
                               flipSecondSegment:Boolean,
                               override val objAfter: Long,override val neighborhood:SegmentExchange,
                               override val neighborhoodName:String = "SegmentExchangeMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName,neighborhood.vrp){

  override def impactedPoints: Iterable[Long] =
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