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


import oscar.cbls.business.routing.model.{ClosestNeighbors, PDP}
import oscar.cbls.core.search.EasyNeighborhood

/**
  * Created by f.germeau on 16/09/2016.
 * THIS is EXPERIMENTAL!!
  */
case class PickupDeliveryCoupleExchange(pdp: PDP with ClosestNeighbors,
                                        neighborhoodName:String = "PickupDeliveryCoupleExchange",
                                        best:Boolean = false,
                                        hotRestart:Boolean = true,
                                        k:Int = 10
                                       )extends EasyNeighborhood[PickupDeliveryCoupleExchangeMove](best,neighborhoodName) {

  /**
    * This variable contains the values used to describe the movement.
    * 0° : The number of the first route used
    * 1° : The pickup node removed from the first route
    * 2° : The delivery node removed from the first route
    * 3° : The insertion position of the pickup node from the second route
    * 4° : The insertion position of the delivery node from the second route
    * 5° : The number of the second route used
    * 6° : The pickup node removed from the second route
    * 7° : The delivery node removed from the second route
    * 8° : The insertion position of the pickup node from the first route
    * 9° : The insertion position of the delivery node from the first route
    */
  val exchange = Array.tabulate(10)(n => 0)

  val seq = pdp.routes

  var startIndice = 0



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

    def customFilter(x:Int,v:Int,p:Int,d:Int): Boolean ={
      pdp.getVehicleOfNode(x) == v && x != p && x != d
    }

    val closestRoutedNeighboursInTime = pdp.computeClosestNeighborInTime()
    for(r1 <- 0 until pdp.v-1){
      exchange(0) = r1
      for(p1 <- pdp.getRoutedPickups.filter(pdp.getVehicleOfNode(_) == r1)){
        exchange(1) = p1
        exchange(2) = pdp.getRelatedDelivery(p1)
        for(r2 <- r1+1 until pdp.v){
          exchange(5) = r2
          for(p2 <- pdp.getRoutedPickups.filter(pdp.getVehicleOfNode(_) == r2)){
            exchange(6) = p2
            exchange(7) = pdp.getRelatedDelivery(p2)
            val closestNeighborsForP1 = pdp.kFirst(k,closestRoutedNeighboursInTime)(p1).filter(customFilter(_,r2,p2,exchange(7)))
            val closestNeighborsForP2 = pdp.kFirst(k,closestRoutedNeighboursInTime)(p2).filter(customFilter(_,r1,p1,exchange(2)))
            val closestNeighborsForD1 = pdp.kFirst(k,closestRoutedNeighboursInTime)(exchange(2)).filter(customFilter(_,r2,p2,exchange(7)))
            val closestNeighborsForD2 = pdp.kFirst(k,closestRoutedNeighboursInTime)(exchange(7)).filter(customFilter(_,r1,p1,exchange(2)))
            for(newPredsP2 <- closestNeighborsForP2){
              for(newPredsD2 <- closestNeighborsForD2){
                for(newPredsP1 <- closestNeighborsForP1){
                  for(newPredsD1 <- closestNeighborsForD1){
                    exchange(3) = newPredsP2
                    exchange(4) = newPredsD2
                    exchange(8) = newPredsP1
                    exchange(9) = newPredsD1

                    doMove(exchange(0), exchange(1), exchange(2),
                      exchange(3), exchange(4),
                      exchange(5), exchange(6), exchange(7),
                      exchange(8), exchange(9))
                    if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjAndRollBack())) {
                      seq.releaseTopCheckpoint()
                      seq.releaseTopCheckpoint()
                      seq.releaseTopCheckpoint()
                      //startIndice = insertionPoint + 1
                      return
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    seq.releaseTopCheckpoint()
  }

  def doMove(firstRoute: Int, firstPickup: Int, firstDelivery: Int,
             secondPickupNewPreds: Int, secondDeliveryNewPreds: Int,
             secondRoute: Int, secondPickup: Int, secondDelivery: Int,
             firstPickupNewPreds: Int, firstDeliveryNewPreds: Int) ={
    assert(firstRoute != secondRoute,"The first and the second route selected must be different")
    assert(pdp.getVehicleOfNode(firstPickup) == firstRoute, "The first pickup node must be removed from the first route")
    assert(pdp.getVehicleOfNode(firstDelivery) == firstRoute, "The first delivery node must be removed from the first route")
    assert(pdp.getVehicleOfNode(secondPickupNewPreds) == firstRoute, "The second pickup insertion location must be on the first route")
    assert(pdp.getVehicleOfNode(secondDeliveryNewPreds) == firstRoute, "The second delivery insertion location must be on the first route")
    assert(pdp.getVehicleOfNode(secondPickup) == secondRoute, "The second pickup node must be removed from the second route")
    assert(pdp.getVehicleOfNode(secondDelivery) == secondRoute, "The second delivery node must be removed from the second route")
    assert(pdp.getVehicleOfNode(firstPickupNewPreds) == secondRoute, "The first pickup insertion location must be on the second route")
    assert(pdp.getVehicleOfNode(firstDeliveryNewPreds) == secondRoute, "The first delivery insertion location must be on the second route")

    seq.remove(seq.newValue.positionOfAnyOccurrence(firstPickup).get)
    seq.remove(seq.newValue.positionOfAnyOccurrence(firstDelivery).get)

    val secondDeliveryPos = seq.newValue.positionOfAnyOccurrence(secondDelivery).get
    val secondDeliveryNewPos = seq.newValue.positionOfAnyOccurrence(secondDeliveryNewPreds).get
    seq.move(secondDeliveryPos,secondDeliveryPos,secondDeliveryNewPos,false)
    val secondPickupPos = seq.newValue.positionOfAnyOccurrence(secondPickup).get
    val secondPickupNewPos = seq.newValue.positionOfAnyOccurrence(secondPickupNewPreds).get
    seq.move(secondPickupPos,secondPickupPos,secondPickupNewPos,false)

    val firstDeliveryNewPos = seq.newValue.positionOfAnyOccurrence(firstDeliveryNewPreds).get
    seq.insertAtPosition(firstDelivery,firstDeliveryNewPos+1)
    val firstPickupNewPos = seq.newValue.positionOfAnyOccurrence(firstPickupNewPreds).get
    seq.insertAtPosition(firstPickup,firstPickupNewPos+1)
  }

  override def instantiateCurrentMove(newObj: Int) =
    PickupDeliveryCoupleExchangeMove(exchange(0), exchange(1), exchange(2),
      exchange(3), exchange(4),
      exchange(5), exchange(6), exchange(7),
      exchange(8), exchange(9),
      newObj,this,neighborhoodName)
}

case class PickupDeliveryCoupleExchangeMove(firstRoute: Int, firstPickup: Int, firstDelivery: Int,
                                            secondPickupNewPreds: Int, secondDeliveryNewPreds: Int,
                                            secondRoute: Int, secondPickup: Int, secondDelivery: Int,
                                            firstPickupNewPreds: Int, firstDeliveryNewPreds: Int,
                                            override val objAfter: Int, override val neighborhood:PickupDeliveryCoupleExchange,
                                            override val neighborhoodName:String = "PickupDeliveryCoupleExchangeMove"
                                           )extends VRPSMove(objAfter, neighborhood, neighborhoodName, neighborhood.pdp){
  override def impactedPoints: Iterable[Int] =
    Iterable(firstRoute,firstPickup,firstDelivery,
      secondPickupNewPreds,secondDeliveryNewPreds,
      secondRoute,secondPickup,secondDelivery,
      firstPickupNewPreds,firstDeliveryNewPreds)

  /** to actually take the move */
  override def commit(): Unit = {
    neighborhood.doMove(firstRoute, firstPickup, firstDelivery,
      secondPickupNewPreds, secondDeliveryNewPreds,
      secondRoute, secondPickup, secondDelivery,
      firstPickupNewPreds, firstDeliveryNewPreds)
  }

  override def toString: String = {
    neighborhoodNameToString + "(FirstRoute:" + firstRoute +
      "; firstPickup:" + firstPickup +
      "; firstDelivery:" + firstDelivery +
      "; beforeSecondPickupInsertion:" + secondPickupNewPreds + "; beforeSecondDeliveryInsertion:" + secondDeliveryNewPreds +
      "; secondRoute:" + secondRoute +
      "; secondPickup:" + secondPickup +
      "; secondDelivery:" + secondDelivery +
      "; beforeFirstPickupInsertion:" + firstPickupNewPreds + "; beforeFirstDeliveryInsertion:" + firstDeliveryNewPreds +
    objToString + ")"
  }
}