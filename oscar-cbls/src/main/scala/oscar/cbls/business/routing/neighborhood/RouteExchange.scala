package oscar.cbls.business.routing.neighborhood

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search.EasyNeighborhood

/**
  * Created by fg on 20/06/17.
  */
class RouteExchange(val vrp: VRP,
                    vehicles:() => Iterable[Int],
                    neighborhoodName:String = "RouteExchange",
                    hotRestart:Boolean = true,
                    best:Boolean = false,
                    tryFlip:Boolean = true) extends EasyNeighborhood[RouteExchangeMove]{

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

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    if(!hotRestart)startVehicle = 0

    for(v1 <- startVehicle to vehicles().size){
      val route1 = vrp.getRouteOfVehicle(v1)
      for(v2 <- v1 to vehicles().size){
        val route2 = vrp.getRouteOfVehicle(v2)
        firstRouteHead = route1.head
        firstRouteLast= route1.last
        secondRouteHead = route2.head
        secondRouteLast = route2.last
        if(tryFlip) {
          for(r1 <- 0 to 2; r2 <- 0 to 2) {
            
            doMove(firstRouteHead, firstRouteLast, secondRouteHead, secondRouteLast)
            if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjAndRollBack())) {
              seq.releaseTopCheckpoint()
              startVehicle = v1 + 1
              return
            }
          }
        }
      }
    }
    seq.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Int): RouteExchangeMove = {
    RouteExchangeMove(
      firstRouteHead, firstRouteLast,
      secondRouteHead, secondRouteLast,
      newObj, this, neighborhoodName)
  }

  def doMove(firstRouteHead: Int, firstRouteLast: Int, secondRouteHead: Int, secondRouteLast: Int){
    seq.swapSegments(firstRouteHead,
      firstRouteLast,
      false,
      secondRouteHead,
      secondRouteLast,
      false)
  }
}


case class RouteExchangeMove(firstRouteHead: Int, firstRouteLast: Int,
                             secondRouteHead: Int, secondRouteLast: Int,
                             override val objAfter: Int,
                             override val neighborhood:RouteExchange,
                             override val neighborhoodName:String = "RouteExchangeMove") extends VRPSMove(objAfter, neighborhood, neighborhoodName,neighborhood.vrp) {
  override def impactedPoints: Iterable[Int] =
    neighborhood.vrp.routes.value.valuesBetweenPositionsQList(firstRouteHead,firstRouteLast) ++
    neighborhood.vrp.routes.value.valuesBetweenPositionsQList(secondRouteHead,secondRouteLast)

  /** to actually take the move */
  override def commit(): Unit = {
    neighborhood.doMove(
      firstRouteHead, firstRouteLast,
      secondRouteHead, secondRouteLast)
  }

  override def toString: String = {
    neighborhoodNameToString + "RouteExchange(firstRouteHead:" + firstRouteHead + " firstRouteLast:" + firstRouteLast +
      " secondRouteHead:" + secondRouteHead + " secondRouteLast:" + secondRouteLast + objToString + ")"
  }
}