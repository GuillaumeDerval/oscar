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
                           relevantNeighbors:()=>Int=>Iterable[Int], //must be routed
                           vehicles:() => Iterable[Int],
                           neighborhoodName:String = "SegmentExchange",
                           hotRestart:Boolean = true,

                           selectFirstVehicleBehavior:LoopBehavior = First(),
                           selectFirstNodeOfFirstSegmentBehavior:LoopBehavior = First(),
                           selectSecondNodeOfFirstSegmentBehavior:LoopBehavior = First(),
                           selectFirstNodeOfSecondSegmentBehavior:LoopBehavior = First(),
                           selectSecondNodeOfSecondSegmentBehavior:LoopBehavior = First(),

                           tryFlip:Boolean = true)
  extends EasyNeighborhoodMultiLevel[SegmentExchangeMove](neighborhoodName) {

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

    val nodeToRoute:Array[Int] = vrp.vehicleOfNode.map(_.value)

    val listOfVehiclesToIterateOn = (if (hotRestart) HotRestart(vehicles(), startVehicle) else vehicles()).toList
    var allVehiclesToIterateOn = SortedSet.empty[Int] ++ listOfVehiclesToIterateOn

    val (listOfVehiclesToIterateOnIterable,notifyFound1) = selectFirstVehicleBehavior.toIterable(listOfVehiclesToIterateOn)
    var firstVehicle = -1

    for(firstVehicleTmp <- listOfVehiclesToIterateOnIterable){

      firstVehicle = firstVehicleTmp

      allVehiclesToIterateOn = allVehiclesToIterateOn - firstVehicle

      val routeOfVehicle1: List[Int] = vrp.getRouteOfVehicle(firstVehicle)

      val routeWithRelevantNeighborsTheirVehicleAndPositionGroupedByVehicles:List[(Int,Int,Map[Int,Iterable[(Int,Int,Int)]])] = routeOfVehicle1.map(node =>
        (node, seqValue.positionOfAnyOccurrence(node).head, relevantNeighborsNow(node)
          .map(node => (node,if(node >=v && nodeToRoute(node)!=n) nodeToRoute(node) else -1))
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
                                     segmentsToExchangeGroupedByVehicle: () => Map[Int, List[(Int,Int)]],
                                     relevantNeighbors:()=>Int=>Iterable[Int], //must be routed
                                     vehicles: () => Iterable[Int],
                                     neighborhoodName:String = "PickupDeliverySegmentExchange",
                                     hotRestart:Boolean = true,

                                     selectFirstVehicleBehavior:LoopBehavior = First(),
                                     selectSecondVehicleBehavior:LoopBehavior = First(),
                                     selectFirstSegmentBehavior:LoopBehavior = First(),
                                     selectSecondSegmentBehavior:LoopBehavior = First()

                                     //tryFlip: Boolean = false,
                                    )
  extends EasyNeighborhoodMultiLevel[SegmentExchangeOnSegmentsMove](neighborhoodName){

  var firstSegmentStartPosition:Int = -1
  var firstSegmentEndPosition:Int = -1
  var secondSegmentStartPosition: Int = -1
  var secondSegmentEndPosition: Int = -1
  var tryFlip: Boolean = false
  var startVehicle = 0

  val n = vrp.n
  val v = vrp.v

  val seq = vrp.routes


  override def exploreNeighborhood(): Unit = {
    val seqValue = seq.defineCurrentValueAsCheckpoint(true)
    val relevantNeighborsNow = Array.tabulate(n)(x => HashSet(relevantNeighbors()(x).toList:_*))
    val segmentsToExchangeGroupedByVehiclesNow = segmentsToExchangeGroupedByVehicle()
    val vehiclesNow = vehicles().toList.sorted

    val routePositionOfNodes = vrp.getGlobalRoutePositionOfAllNode
    val prevNodeOfAllNodes = vrp.getGlobalPrevNodeOfAllNodes
    val nextNodeOfAllNodes = vrp.getGlobalNextNodeOfAllNodes

    val startingNodeToSegment: Map[Int,Map[Int,HashSet[(Int,Int)]]] = segmentsToExchangeGroupedByVehiclesNow.map(x => x._1 -> x._2.groupBy(_._1).map(y => y._1 -> HashSet(y._2:_*)))
    val endingNodeToSegment: Map[Int,Map[Int,HashSet[(Int,Int)]]] = segmentsToExchangeGroupedByVehiclesNow.map(x => x._1 -> x._2.groupBy(_._2).map(y => y._1 -> HashSet(y._2:_*)))

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    def computeAllPotentialSegmentToExchangeWith(segment:(Int,Int),otherVehicle: Int): HashSet[(Int,Int)] ={

      val (start,end) = segment

      // Getting the potential starting nodes whose the previous node is in the relevantNeighbors of the given segment's start
      // We first get the previous node of all segment's start of the other vehicle
      // Then we intersect it with the relevantNeighbor of the segment we want to move
      // Finally we get the next node of the intersection
      val previousOfSegmentStartsOfOtherVehicle = startingNodeToSegment(otherVehicle).keys.map(prevNodeOfAllNodes).toList
      val potentialSecondSegmentStartGivenFirstSegmentStartAndVehicle =
        relevantNeighborsNow(start).toList.intersect(previousOfSegmentStartsOfOtherVehicle).map(nextNodeOfAllNodes)filter(_ >= v)

      // Getting the potential ending nodes that are in the relevantNeighbors of the nextNode of the given segment's end
      // We first get the relevantNeighbors of the node following the end of the segment we want to move
      //    or all node if the segment's end is the last node of the route
      // Then we intersect it with all the segment's end of the other vehicle
      // We have the potential ending node of the second segment
      val potentialSecondSegmentEndGivenFirstSegmentEndAndVehicle =
        (if(nextNodeOfAllNodes(end) >= v) relevantNeighborsNow(nextNodeOfAllNodes(end)).toList
        else (v until n).toList).
          intersect(endingNodeToSegment(otherVehicle).keys.toList)filter(_ >= v)

      // Getting all the segments of the secondVehicle that fit the potentialSecondSegment's start and end
      val potentialSecondSegments =
        potentialSecondSegmentStartGivenFirstSegmentStartAndVehicle.flatMap(startingNodeToSegment(otherVehicle)).
          intersect(potentialSecondSegmentEndGivenFirstSegmentEndAndVehicle.flatMap(endingNodeToSegment(otherVehicle)))

      HashSet(potentialSecondSegments:_*)
    }

    if(!hotRestart)startVehicle = 0

    val (listOfVehicleForFirstVehicle,notifyFound1) = selectFirstVehicleBehavior.toIterable(vehiclesNow)
    for(firstVehicle <- listOfVehicleForFirstVehicle){
      val (listOfVehicleForSecondVehicle,notifyFound2) = selectSecondVehicleBehavior.toIterable(vehiclesNow.dropWhile(_ <= firstVehicle))
      for(secondVehicle <- listOfVehicleForSecondVehicle){
        val potentialSegmentsToExchangeWithForSegmentsOfFirstVehicle = segmentsToExchangeGroupedByVehiclesNow(firstVehicle).map(segment => segment -> computeAllPotentialSegmentToExchangeWith(segment,secondVehicle)).toMap
        val potentialSegmentsToExchangeWithForSegmentsOfSecondVehicle = segmentsToExchangeGroupedByVehiclesNow(secondVehicle).map(segment => segment -> computeAllPotentialSegmentToExchangeWith(segment,firstVehicle)).toMap
/*
        println("firstVehicle : ")
        for(seg <- potentialSegmentsToExchangeWithForSegmentsOfFirstVehicle) {
          println("Key : " + seg._1 + "         Value : " + seg._2)
        }

        println("secondVehicle : ")
        for(seg <- potentialSegmentsToExchangeWithForSegmentsOfSecondVehicle) {
          println("Key : " + seg._1 + "            Value : " + seg._2)
        }*/

        val relevantSegmentsCouplesToSwitch =
          if(potentialSegmentsToExchangeWithForSegmentsOfFirstVehicle.isEmpty ||
            potentialSegmentsToExchangeWithForSegmentsOfSecondVehicle.isEmpty)
            List.empty
          else
            Pairs.zipIntoAllPossiblePairs(
            potentialSegmentsToExchangeWithForSegmentsOfFirstVehicle.keys.toList,
            potentialSegmentsToExchangeWithForSegmentsOfSecondVehicle.keys.toList,
            (s1:(Int,Int),s2:(Int,Int)) => {
              //println("s1 : " + s1 + "    s2 : " + s2 + "    segments1 : " + potentialSegmentsToExchangeWithForSegmentsOfFirstVehicle(s1) + "    segments2 : " + potentialSegmentsToExchangeWithForSegmentsOfSecondVehicle(s2))
              potentialSegmentsToExchangeWithForSegmentsOfFirstVehicle(s1).contains(s2) &&
                potentialSegmentsToExchangeWithForSegmentsOfSecondVehicle(s2).contains(s1)
            })

        val (listOfSegmentsCouplesToSwitch,notifyFound3) = selectFirstSegmentBehavior.toIterable(relevantSegmentsCouplesToSwitch)
        for((firstSegment, secondSegment) <- listOfSegmentsCouplesToSwitch){
          firstSegmentStartPosition = routePositionOfNodes(firstSegment._1)
          firstSegmentEndPosition = routePositionOfNodes(firstSegment._2)
          secondSegmentStartPosition = routePositionOfNodes(secondSegment._1)
          secondSegmentEndPosition = routePositionOfNodes(secondSegment._2)

          doMove(firstSegmentStartPosition, firstSegmentEndPosition, false, secondSegmentStartPosition, secondSegmentEndPosition, false)
          if (evaluateCurrentMoveObjTrueIfSomethingFound(evalObjAndRollBack())) {
            notifyFound1()
            notifyFound2()
            notifyFound3()
          }
        }
      }
    }

    seq.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Int): SegmentExchangeOnSegmentsMove = {
    SegmentExchangeOnSegmentsMove(
      firstSegmentStartPosition, firstSegmentEndPosition, false,
      secondSegmentStartPosition, secondSegmentEndPosition, false,
      newObj, this, neighborhoodName)
  }

  def doMove(firstSegmentStartPosition:Int, firstSegmentEndPosition:Int, flipFirstSegment: Boolean,
             secondSegmentStartPosition: Int, secondSegmentEndPosition: Int, flipSecondSegment: Boolean){
    seq.swapSegments(firstSegmentStartPosition,
      firstSegmentEndPosition,
      flipFirstSegment,
      secondSegmentStartPosition,
      secondSegmentEndPosition,
      flipSecondSegment)
  }
}

case class SegmentExchangeOnSegmentsMove(firstSegmentStartPosition:Int,
                               firstSegmentEndPosition:Int,
                               flipFirstSegment:Boolean,
                               secondSegmentStartPosition: Int,
                               secondSegmentEndPosition: Int,
                               flipSecondSegment:Boolean,
                               override val objAfter: Int,override val neighborhood:SegmentExchangeOnSegments,
                               override val neighborhoodName:String = "SegmentExchangeOnSegmentsMove")
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