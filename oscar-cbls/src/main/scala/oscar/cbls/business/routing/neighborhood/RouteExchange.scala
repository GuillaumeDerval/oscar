package oscar.cbls.business.routing.neighborhood

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search.EasyNeighborhood

/**
  * Created by fg on 20/06/17.
  */
case class RouteExchange(val vrp: VRP,
                    vehicles: Array[Int],
                    neighborhoodName:String = "RouteExchange",
                    hotRestart:Boolean = true,
                    best:Boolean = false,
                    tryFlip:Boolean = false) extends EasyNeighborhood[RouteExchangeMove]{

  val seq = vrp.routes
  var startVehicle = 0

  var firstRouteHead = -1
  var firstRouteLast = -1
  var secondRouteHead = -1
  var secondRouteLast = -1
  var firstRouteFlip = false
  var secondRouteFlip = false

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(): Unit = {
    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    val vehiclesNow = vehicles.filter(vrp.getRouteOfVehicle(_).size > 1)
    val routePositions = vrp.routes.value.toList.zipWithIndex.toMap

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    if(!hotRestart)startVehicle = 0

    for(v1 <- startVehicle until vehiclesNow.length){
      val route1 = vrp.getRouteOfVehicle(vehiclesNow(v1))
      for(v2 <- v1+1 until vehiclesNow.length){
        val route2 = vrp.getRouteOfVehicle(vehiclesNow(v2))
        firstRouteHead = routePositions(route1.tail.head)
        firstRouteLast= routePositions(route1.last)
        secondRouteHead = routePositions(route2.tail.head)
        secondRouteLast = routePositions(route2.last)
        if(tryFlip) {
          for(r1 <- 0 until 2; r2 <- 0 until 2) {
            firstRouteFlip = r1 == 1
            secondRouteFlip = r2 == 1
            doMove(firstRouteHead, firstRouteLast, firstRouteFlip, secondRouteHead, secondRouteLast, secondRouteFlip)
            if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjAndRollBack())) {
              seq.releaseTopCheckpoint()
              startVehicle = v1 + 1
              return
            }
          }
        }
        else{
          doMove(firstRouteHead, firstRouteLast, firstRouteFlip, secondRouteHead, secondRouteLast, secondRouteFlip)
          if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjAndRollBack())) {
            seq.releaseTopCheckpoint()
            startVehicle = v1 + 1
            return
          }
        }
      }
    }
    seq.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Int): RouteExchangeMove = {
    RouteExchangeMove(
      firstRouteHead, firstRouteLast, firstRouteFlip,
      secondRouteHead, secondRouteLast, secondRouteFlip,
      newObj, this, neighborhoodName)
  }

  def doMove(firstRouteHead: Int, firstRouteLast: Int, firstFlip: Boolean,
             secondRouteHead: Int, secondRouteLast: Int, secondFlip: Boolean){
    seq.swapSegments(firstRouteHead,
      firstRouteLast,
      firstFlip,
      secondRouteHead,
      secondRouteLast,
      secondFlip)
  }
}


case class RouteExchangeMove(firstRouteHead: Int, firstRouteLast: Int, firstFlip: Boolean,
                             secondRouteHead: Int, secondRouteLast: Int, secondFlip: Boolean,
                             override val objAfter: Int,
                             override val neighborhood:RouteExchange,
                             override val neighborhoodName:String = "RouteExchangeMove") extends VRPSMove(objAfter, neighborhood, neighborhoodName,neighborhood.vrp) {
  override def impactedPoints: Iterable[Int] =
    neighborhood.vrp.routes.value.valuesBetweenPositionsQList(firstRouteHead,firstRouteLast) ++
    neighborhood.vrp.routes.value.valuesBetweenPositionsQList(secondRouteHead,secondRouteLast)

  /** to actually take the move */
  override def commit(): Unit = {
    neighborhood.doMove(
      firstRouteHead, firstRouteLast, firstFlip,
      secondRouteHead, secondRouteLast, secondFlip)
  }

  override def toString: String = {
    neighborhoodNameToString + "RouteExchange(firstRouteHead:" + firstRouteHead + " firstRouteLast:" + firstRouteLast +
      " secondRouteHead:" + secondRouteHead + " secondRouteLast:" + secondRouteLast + objToString + ")"
  }
}