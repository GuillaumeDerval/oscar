package oscar.cbls.business.routing.neighborhood

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior}

import scala.collection.immutable.SortedSet

/**
 * a neighborhood that exchanges the route of two vehicles.
 * It does not modifies the routes themselves. It just exchanges the vehicles
 * @author renaud.delandtsheer@cetic.be
 */
case class RouteMerge(firstVehicles:()=>Iterable[Int],
                      relevantNeighbors:()=>Int=>Iterable[Int], //must be routed
                      vrp:VRP,
                      neighborhoodName: String = "RouteMerge",
                      selectFirstVehicleBehavior:LoopBehavior = First(),
                      selectSecondVehicleBehavior:LoopBehavior = First(),
                      secondVehiclePostFilter:() => Int => Int => Boolean,
                      hotRestart: Boolean = true,
                      breakSymmetriesAmongVehicles:Boolean = true,
                      skipFirstVehicleIfEmptyRoute:Boolean = false)
  extends EasyNeighborhoodMultiLevel[RouteExchangeMove](neighborhoodName) {

  val seq = vrp.routes
  val v = vrp.v
  val n = vrp.n

  //the indice to start with for the exploration
  var startFirstVehicle: Int = 0

  var firstVehicle:Int = -1
  var secondVehicle:Int = -1
  var positionOfFirstVehicle:Int = -1
  var positionOfVehicleNextToFirst:Int = -1
  var firstVehicleIsEmpty:Boolean = false
  var positionOfSecondVehicle:Int = -1
  var positionOfVehicleNextToSecond:Int = -1
  var secondVehicleIsEmpty:Boolean = false

  override def exploreNeighborhood() {

    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    val relevantNeighborsNow = relevantNeighbors()

    val nodeToRoute:Array[Int] = vrp.getVehicleOfAllNodes

    val (firstVehicleIterable,notifyFound1) = selectFirstVehicleBehavior.toIterable(
      if (hotRestart) HotRestart(firstVehicles(), startFirstVehicle) else firstVehicles())

    var firstVehicle = -1

    val secondVehiclePostFilterNow = secondVehiclePostFilter()

    for(firstVehicleTmp <- firstVehicleIterable){

      firstVehicle = firstVehicleTmp

      val secondVehiclePostFilterNowForFirstVehicle = secondVehiclePostFilterNow(firstVehicle)

      val routeOfVehicle1 = vrp.getRouteOfVehicle(firstVehicle)

      val routeWithRelevantNeighborsTheirVehicleAndPositionGroupedByVehicles:List[(Int,Int,Map[Int,Iterable[(Int,Int,Int)]])] = routeOfVehicle1.map(node =>
        (node, seqValue.positionOfAnyOccurrence(node).head, relevantNeighborsNow(node)
          .map(node => (node,if(node >=v && nodeToRoute(node)!=n) nodeToRoute(node) else -1))
          .filter({case (node,routeNr) => node >= v && secondVehiclePostFilterNowForFirstVehicle(routeNr)})
          .map(nodeAndRoute => (nodeAndRoute._1,nodeAndRoute._2,seqValue.positionOfAnyOccurrence(nodeAndRoute._1).head))
          .groupBy(nodeAndRoute => nodeAndRoute._2))
      )

      val (firstVehicleIt,notifyFound1) = selectFirstVehicleBehavior.toIterator(iterationSchemeOnZone)
      while (firstVehicleIt.hasNext) {
        firstVehicle = firstVehicleIt.next()

        require(firstVehicle < v,"first vehicle is not <v:" + firstVehicle)

        positionOfFirstVehicle = startValue.positionOfAnyOccurrence(firstVehicle).get
        positionOfVehicleNextToFirst = if(firstVehicle == v-1) startValue.size else startValue.positionOfAnyOccurrence(firstVehicle+1).get
        firstVehicleIsEmpty = positionOfFirstVehicle+1 == positionOfVehicleNextToFirst

        if(!skipFirstVehicleIfEmptyRoute || !firstVehicleIsEmpty){

          val secondVehiclesZone = secondVehiclesNow(firstVehicle)
          val (secondVehicleIt,notifyFound2) = selectSecondVehicleBehavior.toIterator(secondVehiclesZone)
          while (secondVehicleIt.hasNext) {
            secondVehicle = secondVehicleIt.next()
            require(firstVehicle < v,"first vehicle is not <v:" + firstVehicle)
            if(secondVehicle != firstVehicle && (!breakSymmetriesAmongVehicles || secondVehicle > firstVehicle)){

              positionOfSecondVehicle = startValue.positionOfAnyOccurrence(secondVehicle).get
              positionOfVehicleNextToSecond = if(secondVehicle == v-1) startValue.size else startValue.positionOfAnyOccurrence(secondVehicle+1).get
              secondVehicleIsEmpty = positionOfSecondVehicle +1 == positionOfVehicleNextToSecond

              if(!firstVehicleIsEmpty || !secondVehicleIsEmpty){

                doMove(positionOfFirstVehicle,positionOfVehicleNextToFirst,firstVehicleIsEmpty,
                  positionOfSecondVehicle,positionOfVehicleNextToSecond,secondVehicleIsEmpty)

                if(evaluateCurrentMoveObjTrueIfSomethingFound(evalObjAndRollBack())) {
                  notifyFound1()
                  notifyFound2()
                }
              }
            }
          }
        }
      }
      seq.releaseTopCheckpoint()
      //For the hotRestart
      startFirstVehicle = firstVehicle + 1

      //to ensure that we do not access these values without assigning them first, we set a trap value
      firstVehicle = -1
      secondVehicle = -1
      positionOfFirstVehicle = -1
      positionOfVehicleNextToFirst = -1
      positionOfSecondVehicle = -1
      positionOfVehicleNextToSecond = -1
    }


    override def instantiateCurrentMove(newObj: Int) =
    RouteExchangeMove(firstVehicle:Int,
      positionOfFirstVehicle:Int,
      positionOfVehicleNextToFirst:Int,
      firstVehicleIsEmpty:Boolean,
      secondVehicle:Int,
      positionOfSecondVehicle:Int,
      positionOfVehicleNextToSecond:Int,
      secondVehicleIsEmpty:Boolean,
      newObj, this, neighborhoodName)

    override def reset(): Unit = {
      startFirstVehicle = 0
    }

    def doMove(positionOfFirstVehicle:Int,
               positionOfVehicleNextToFirst:Int,
               firstVehicleIsEmpty:Boolean,
               positionOfSecondVehicle:Int,
               positionOfVehicleNextToSecond:Int,
               secondVehicleIsEmpty:Boolean) {
      if (firstVehicleIsEmpty) {
        if (secondVehicleIsEmpty) {
          System.err.println("doing Route exchange with two empty routes")
        } else {
          seq.move(positionOfSecondVehicle + 1, positionOfVehicleNextToSecond - 1, positionOfFirstVehicle, false)
        }
      } else {
        if (secondVehicleIsEmpty) {
          seq.move(positionOfFirstVehicle + 1, positionOfVehicleNextToFirst - 1, positionOfSecondVehicle, false)
        } else {
          seq.swapSegments(positionOfFirstVehicle + 1, positionOfVehicleNextToFirst - 1, false,
            positionOfSecondVehicle + 1, positionOfVehicleNextToSecond - 1, false)
        }
      }
    }
  }

  /**
   * @author renaud.delandtsheer@cetic.be
   */
  case class RouteMergeMove(firstVehicle:Int,
                            positionOfFirstVehicle:Int,
                            positionOfVehicleNextToFirst:Int,
                            firstVehicleIsEmpty:Boolean,
                            secondVehicle:Int,
                            positionOfSecondVehicle:Int,
                            positionOfVehicleNextToSecond:Int,
                            secondVehicleIsEmpty:Boolean,
                            override val objAfter: Int,
                            override val neighborhood: RouteExchange,
                            override val neighborhoodName: String = "RouteExchangeMove")
    extends VRPSMove(objAfter, neighborhood, neighborhoodName, neighborhood.vrp){

    override def impactedPoints: Iterable[Int] = ???

    override def commit() {
      neighborhood.doMove(positionOfFirstVehicle,
        positionOfVehicleNextToFirst,
        firstVehicleIsEmpty,
        positionOfSecondVehicle,
        positionOfVehicleNextToSecond,
        secondVehicleIsEmpty)
    }

    override def toString: String =
      neighborhoodNameToString + "RouteExchangeMove(vehicle:" + firstVehicle + " other vehicle:" + secondVehicle + objToString + ")"
  }

