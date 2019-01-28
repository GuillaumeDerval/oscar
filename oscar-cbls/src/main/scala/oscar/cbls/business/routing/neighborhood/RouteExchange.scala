package oscar.cbls.business.routing.neighborhood

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior}
import oscar.cbls._


/**
 * a neighborhood that exchanges the route of two vehicles.
 * It does not modifies the routes themselves. It just exchanges the vehicles
 * @author renaud.delandtsheer@cetic.be
 */
case class RouteExchange(firstVehicles:()=>Iterable[Long],
                         secondVehicles:()=>Long=>Iterable[Long],
                         vrp:VRP,
                         neighborhoodName: String = "RouteExchange",
                         selectFirstVehicleBehavior:LoopBehavior = First(),
                         selectSecondVehicleBehavior:LoopBehavior = First(),
                         hotRestart: Boolean = true,
                         breakSymmetriesAmongVehicles:Boolean = true,
                         skipFirstVehicleIfEmptyRoute:Boolean = false)
  extends EasyNeighborhoodMultiLevel[RouteExchangeMove](neighborhoodName) {

  val seq = vrp.routes
  val v = vrp.v
  val n = vrp.n

  //the indice to start with for the exploration
  var startFirstVehicle: Long = 0

  var firstVehicle:Long = -1L
  var secondVehicle:Long = -1L
  var positionOfFirstVehicle:Long = -1L
  var positionOfVehicleNextToFirst:Long = -1L
  var firstVehicleIsEmpty:Boolean = false
  var positionOfSecondVehicle:Long = -1L
  var positionOfVehicleNextToSecond:Long = -1L
  var secondVehicleIsEmpty:Boolean = false

  override def exploreNeighborhood(initialObj: Long){

    val iterationSchemeOnZone =
      if (hotRestart) HotRestart(firstVehicles(), startFirstVehicle)
      else firstVehicles()

    val startValue = seq.defineCurrentValueAsCheckpoint(true)

    def evalObjAndRollBack() : Long = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(startValue)
      a
    }

    val secondVehiclesNow = secondVehicles()

    val (firstVehicleIt,notifyFound1) = selectFirstVehicleBehavior.toIterator(iterationSchemeOnZone)
    while (firstVehicleIt.hasNext) {
      firstVehicle = firstVehicleIt.next()

      require(firstVehicle < v,"first vehicle is not <v:" + firstVehicle)

      positionOfFirstVehicle = startValue.positionOfAnyOccurrence(firstVehicle).get
      positionOfVehicleNextToFirst = if(firstVehicle == v-1L) startValue.size else startValue.positionOfAnyOccurrence(firstVehicle+1L).get
      firstVehicleIsEmpty = positionOfFirstVehicle+1L == positionOfVehicleNextToFirst

      if(!skipFirstVehicleIfEmptyRoute || !firstVehicleIsEmpty){

        val secondVehiclesZone = secondVehiclesNow(firstVehicle)
        val (secondVehicleIt,notifyFound2) = selectSecondVehicleBehavior.toIterator(secondVehiclesZone)
        while (secondVehicleIt.hasNext) {
          secondVehicle = secondVehicleIt.next()
          require(firstVehicle < v,"first vehicle is not <v:" + firstVehicle)

          if(!breakSymmetriesAmongVehicles || secondVehicle > firstVehicle){

            positionOfSecondVehicle = startValue.positionOfAnyOccurrence(secondVehicle).get
            positionOfVehicleNextToSecond = if(secondVehicle == v-1L) startValue.size else startValue.positionOfAnyOccurrence(secondVehicle+1L).get
            secondVehicleIsEmpty = positionOfSecondVehicle +1L == positionOfVehicleNextToSecond

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
    startFirstVehicle = firstVehicle + 1L

    //to ensure that we do not access these values without assigning them first, we set a trap value
    firstVehicle = -1L
    secondVehicle = -1L
    positionOfFirstVehicle = -1L
    positionOfVehicleNextToFirst = -1L
    positionOfSecondVehicle = -1L
    positionOfVehicleNextToSecond = -1L
  }


  override def instantiateCurrentMove(newObj: Long) =
    RouteExchangeMove(firstVehicle:Long,
      positionOfFirstVehicle:Long,
      positionOfVehicleNextToFirst:Long,
      firstVehicleIsEmpty:Boolean,
      secondVehicle:Long,
      positionOfSecondVehicle:Long,
      positionOfVehicleNextToSecond:Long,
      secondVehicleIsEmpty:Boolean,
      newObj, this, neighborhoodName)

  override def reset(): Unit = {
    startFirstVehicle = 0L
  }

  def doMove(positionOfFirstVehicle:Long,
             positionOfVehicleNextToFirst:Long,
             firstVehicleIsEmpty:Boolean,
             positionOfSecondVehicle:Long,
             positionOfVehicleNextToSecond:Long,
             secondVehicleIsEmpty:Boolean) {
    if (firstVehicleIsEmpty) {
      if (secondVehicleIsEmpty) {
        System.err.println("doing Route exchange with two empty routes")
      } else {
        seq.move(positionOfSecondVehicle + 1L, positionOfVehicleNextToSecond - 1L, positionOfFirstVehicle, false)
      }
    } else {
      if (secondVehicleIsEmpty) {
        seq.move(positionOfFirstVehicle + 1L, positionOfVehicleNextToFirst - 1L, positionOfSecondVehicle, false)
      } else {
        seq.swapSegments(positionOfFirstVehicle + 1L, positionOfVehicleNextToFirst - 1L, false,
          positionOfSecondVehicle + 1L, positionOfVehicleNextToSecond - 1L, false)
      }
    }
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
case class RouteExchangeMove(firstVehicle:Long,
                             positionOfFirstVehicle:Long,
                             positionOfVehicleNextToFirst:Long,
                             firstVehicleIsEmpty:Boolean,
                             secondVehicle:Long,
                             positionOfSecondVehicle:Long,
                             positionOfVehicleNextToSecond:Long,
                             secondVehicleIsEmpty:Boolean,
                             override val objAfter: Long,
                             override val neighborhood: RouteExchange,
                             override val neighborhoodName: String = "RouteExchangeMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName, neighborhood.vrp){

  //TODO: Implement this
  override def impactedPoints: Iterable[Long] = ???

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

