package oscar.cbls.test.routing

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


import oscar.cbls._
import oscar.cbls.business.routing.model._
import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.core.objective.{CascadingObjective, Objective}
import oscar.cbls.core.search.{Best, CompositeMove}
import oscar.cbls.lib.invariant.seq.Length
import oscar.cbls.lib.search.combinators._

/**
 * Created by fabian on 04-07-16.
 */

class MyPDP(n:Int, v:Int, m:Store,
            symmetricDistance:Array[Array[Int]], maxPivot:Int,
            pickups:Array[Int], deliveries:Array[Int],
            timeWindows:Array[(Int,Int,Int,Int)], ttf:TTFMatrix)
  extends PDP(n,v,m,maxPivot) with ConstantDistancePerVehicle with ClosestNeighbors {

  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  val penaltyForUnrouted  = 100000




  def closestNeighboursInTime = computeClosestNeighborInTime()

  def size = routes.value.size




  addPickupDeliveryCouples(pickups,deliveries)
  setArrivalLeaveLoadValue()
  setVehiclesMaxCargo(5)
  setVehiclesCapacityStrongConstraint()
  setTimeWindows(timeWindows)
  setTravelTimeFunctions(ttf)

  /**
   * Redefine the toString method.
   * @return the VRP problem as a String.
   */
  override def toString : String =
    super.toString +
      "objective: " + obj.value + "\n" +
      "next:" + next.map(_.value).mkString(",") + "\n" +
      "prev:" + prev.map(_.value).mkString(",") + "\n"


  val obj = new CascadingObjective(fastConstraints,
    new CascadingObjective(slowConstraints,
      new CascadingObjective(precedenceObj, Objective(totalDistance + (penaltyForUnrouted*(n - Length(routes)))))))
}

object PickupDeliveryS extends App{
  val n = 110
  val v = 10

  val maxPivotPerValuePercent = 4

  println("PDP(n:" + n + " v:" + v + ")")

  val routingMatrix = RoutingMatrixGenerator(n)

  val symmetricDistanceMatrix = routingMatrix._1

  val model = new Store(noCycle = false)

  val (pickups,deliveries) = RoutingMatrixGenerator.generatePickupDeliveryCouples(n,v)

  val timeWindows = RoutingMatrixGenerator.generateTimeWindows(n, v, pickups, deliveries)

  val ttf = RoutingMatrixGenerator.generateLinearTravelTimeFunction(n,symmetricDistanceMatrix)

  val myPDP = new MyPDP(n,v,model,symmetricDistanceMatrix,maxPivotPerValuePercent,pickups,deliveries,timeWindows,ttf)
  val nodes = myPDP.nodes



  model.close()

  println(model.stats)

  val insertCoupleFast = Profile(dynAndThen(
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => myPDP.getUnroutedPickups,
      relevantPredecessor = ()=>myPDP.kFirst(n/10,myPDP.closestNeighboursInTime,myPDP.isRouted),
      vrp = myPDP),
    (moveResult:InsertPointMove) => InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => Iterable(myPDP.getRelatedDelivery(moveResult.insertedPoint)),
      relevantPredecessor = () => myPDP.getNodesAfterRelatedPickup(),
      vrp = myPDP,selectNodeBehavior = Best(),selectInsertionPointBehavior = Best()))name "insertCoupleFast")

  val insertCoupleSlow = Profile(dynAndThen(
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => myPDP.getUnroutedPickups,
      relevantPredecessor = ()=> myPDP.kFirst(n/5,myPDP.closestNeighboursInTime,myPDP.isRouted),
      vrp = myPDP),
    (moveResult:InsertPointMove) => InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => Iterable(myPDP.getRelatedDelivery(moveResult.insertedPoint)),
      relevantPredecessor = () => myPDP.getNodesAfterRelatedPickup(),
      vrp = myPDP,selectNodeBehavior = Best(),selectInsertionPointBehavior = Best()))name "insertCoupleSlow")
  /*

    val insertCoupleCloseToDepot = Profile(dynAndThen(
      InsertPointUnroutedFirst(
        unroutedNodesToInsert = () => {
          var tempList:List[Int] = List.empty
          for(i <- 0 until v){
            tempList = myPDP.kFirst(n/v,myPDP.closestNeighboursForward,myPDP.isUnroutedPickup)(i).toList ::: tempList
          }
          tempList
        },
        relevantPredecessor = () => myPDP.kFirst(n/10,myPDP.closestNeighboursInTime,myPDP.isRouted),
        vrp = myPDP),
      (moveResult:InsertPointMove) => InsertPointUnroutedFirst(
        unroutedNodesToInsert =  () => Iterable(myPDP.getRelatedDelivery(moveResult.insertedPoint)),
        relevantPredecessor = () => myPDP.getNodesAfterRelatedPickup(),
        vrp = myPDP, best = true))name "insertCoupleCloseToDepot")*/

  val oneCoupleMove = Profile(dynAndThen(OnePointMove(
    nodesToMove = () => myPDP.getRoutedPickups,
    relevantNewPredecessors= () => myPDP.kFirst(20,myPDP.closestNeighboursInTime,myPDP.isRouted),
    vrp = myPDP),
    (moveResult:OnePointMoveMove) => OnePointMove(
      nodesToMove = () => List(myPDP.getRelatedDelivery(moveResult.movedPoint)),
      relevantNewPredecessors= () => myPDP.getNodesAfterRelatedPickup(),
      vrp = myPDP,
      selectPointToMoveBehavior= Best(),
      selectDestinationBehavior=Best())) name "oneCoupleMove")

  val onePointMovePD = Profile(new RoundRobin(List(OnePointMove(
    nodesToMove = () => myPDP.getRoutedPickups,
    relevantNewPredecessors = () => myPDP.getNodesBeforeRelatedDelivery(),
    vrp = myPDP),OnePointMove(
    nodesToMove = () => myPDP.getRoutedDeliverys,
    relevantNewPredecessors = () => myPDP.getNodesAfterRelatedPickup(),
    vrp = myPDP)))name "OnePointMove PickupDelivery")

  val pickupDeliveryCoupleShift = Profile(dynAndThen(OnePointMove(
    nodesToMove = () => myPDP.getRoutedPickups,
    relevantNewPredecessors = () => myPDP.kFirst(20,myPDP.closestNeighboursInTime,myPDP.isRouted),
    vrp = myPDP),
    (moveResult:OnePointMoveMove) => OnePointMove(
      nodesToMove = () => List(myPDP.getRelatedDelivery(moveResult.movedPoint)),
      relevantNewPredecessors = () => myPDP.getNodesAfterRelatedPickup(),
      vrp = myPDP,
      selectPointToMoveBehavior= Best(),
      selectDestinationBehavior=Best()))name "pickupDeliveryCoupleShift")

  val removeCouple = Profile(dynAndThen(new RemovePoint(
    relevantPointsToRemove = () => myPDP.getRoutedPickups,
    vrp = myPDP),
    (moveResult1:RemovePointMove) =>
      new RemovePoint(
        relevantPointsToRemove = () => List(myPDP.getRelatedDelivery(moveResult1.pointToRemove)),
        vrp = myPDP)
    , maximalIntermediaryDegradation = Int.MaxValue)
  )

  val smartInsertCoupleNotFair = Profile(dynAndThen(
    dynAndThen(InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => myPDP.getUnroutedPickups,
      relevantPredecessor = () => myPDP.kFirst(n/10, myPDP.closestNeighboursInTime,myPDP.isRouted),
      vrp = myPDP
    ),
      (moveResult1:InsertPointMove) => {
        RemovePoint(
          relevantPointsToRemove = () => {
            val nextNode = myPDP.routes.value.valueAtPosition(moveResult1.insertAtPosition+1)
            if (nextNode.isDefined && nextNode.get >= v)
              Iterable(nextNode.get)
            else
              Iterable.empty
          },
          vrp = myPDP
        )
      }
    ),
    (moveResult2:CompositeMove) =>{
      dynAndThen(RemovePoint(
        relevantPointsToRemove = () => Iterable(myPDP.getRelatedNode(moveResult2.ml(1).asInstanceOf[RemovePointMove].pointToRemove)),
        vrp = myPDP
      ),
        (moveResult3: RemovePointMove) => InsertPointUnroutedFirst(
          unroutedNodesToInsert = () => Iterable(myPDP.getRelatedDelivery(moveResult2.ml.head.asInstanceOf[InsertPointMove].insertedPoint)),
          relevantPredecessor = () => myPDP.getNodesAfterRelatedPickup(),
          vrp = myPDP
        ))
    })name "smartInsertCoupleNotFair")

  val insertCoupleNotFair = Profile(dynAndThen(
    dynAndThen(RemovePoint(
      relevantPointsToRemove = () => myPDP.getRoutedPickups,
      vrp = myPDP
    ),
      (moveResult1:RemovePointMove) => RemovePoint(
        relevantPointsToRemove = () => Iterable(myPDP.getRelatedDelivery(moveResult1.pointToRemove)),
        vrp = myPDP
      )
    ),
    (moveResult2:CompositeMove) =>{
      val removedPickup = (moveResult2.ml.head.asInstanceOf[RemovePointMove].pointToRemove,
        moveResult2.ml.head.asInstanceOf[RemovePointMove].positionOfPointToRemove)
      val removedDelivery = (myPDP.getRelatedDelivery(removedPickup._1),
        moveResult2.ml(1).asInstanceOf[RemovePointMove].positionOfPointToRemove)
      dynAndThen(InsertPointUnroutedFirst(
        unroutedNodesToInsert = () => myPDP.getUnroutedDeliverys.filter(_ != removedDelivery._1),
        relevantPredecessor = () => (x:Int) => Iterable(myPDP.routes.value.valueAtPosition(removedDelivery._2-1).get),
        vrp = myPDP
      ),
        (moveResult3:InsertPointMove) => InsertPointUnroutedFirst(
          unroutedNodesToInsert = () => Iterable(myPDP.getRelatedPickup(moveResult3.insertedPoint)),
          relevantPredecessor = () => (x:Int) => Iterable(myPDP.routes.value.valueAtPosition(removedPickup._2-1).get),
          vrp = myPDP
        )
      )}
  )name "insertCoupleNotFair")

  val pickupDeliveryCoupleExchange = Profile(PickupDeliveryCoupleExchange(myPDP,k = 5)name "PD-CoupleExchange")

  val pickupDeliverySegmentExchange = Profile(PickupDeliverySegmentExchange(myPDP, relevantNeighbors = ()=>myPDP.kFirst((n-v)/2,myPDP.closestNeighboursInTime),hotRestart = false)name "PD-SegmentExchange")

  def dynAndThenCoupleExchange = {
    var firstVehicle = 10
    var secondVehicle = 10
    Profile(
      dynAndThen(
        //We first remove a PD couple from a first route
        dynAndThen(new RemovePoint(
          relevantPointsToRemove = () => {
            myPDP.getRoutedPickups
          },
          vrp = myPDP),
          (moveResult1:RemovePointMove) =>{
            firstVehicle = myPDP.getVehicleOfNode(myPDP.getRelatedDelivery(moveResult1.pointToRemove))
            new RemovePoint(
              relevantPointsToRemove = () => List(myPDP.getRelatedDelivery(moveResult1.pointToRemove)),
              vrp = myPDP)}
        )
        ,(moveResult2:CompositeMove) =>{
          //Then we move a PD couple from a second route to the first route
          dynAndThen(
            dynAndThen(OnePointMove(
              nodesToMove = () => myPDP.notOnSameVehicle(myPDP.getRoutedPickups,firstVehicle),
              relevantNewPredecessors = () => (i:Int) => myPDP.getNodesOfVehicle(firstVehicle),
              vrp = myPDP), (moveResult3:OnePointMoveMove) =>{
              secondVehicle = myPDP.getVehicleOfNode(myPDP.getRelatedDelivery(moveResult3.movedPoint))
              OnePointMove(
                nodesToMove = () => List(myPDP.getRelatedDelivery(moveResult3.movedPoint)),
                relevantNewPredecessors= () => myPDP.getNodesAfterRelatedPickup(),
                vrp = myPDP,
                selectPointToMoveBehavior= Best(),
                selectDestinationBehavior=Best())}
            )
            ,(moveResult4:CompositeMove) =>{
              //And finally we insert the first couple in the second route
              dynAndThen(InsertPointUnroutedFirst(
                unroutedNodesToInsert = () => {
                  Iterable(moveResult2.ml.head.asInstanceOf[RemovePointMove].pointToRemove)
                },
                relevantPredecessor = () => (i:Int) => myPDP.getNodesOfVehicle(secondVehicle),
                vrp = myPDP), (moveResult5:InsertPointMove) =>{
                InsertPointUnroutedFirst(
                  unroutedNodesToInsert = () => Iterable(myPDP.getRelatedDelivery(moveResult5.insertedPoint)),
                  relevantPredecessor = () => myPDP.getNodesAfterRelatedPickup(),
                  vrp = myPDP, selectNodeBehavior = Best(),selectInsertionPointBehavior = Best())}
              )}
            , maximalIntermediaryDegradation = Int.MaxValue/2
          )}
      )name "dynAndThenCoupleExchange"
    )
  }

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(() => nodes, ()=>myPDP.kFirst(k,myPDP.closestNeighboursInTime), myPDP,breakSymmetry = breakSym,neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val treeopt = threeOpt(n/10,true)



  val search = BestSlopeFirst(List(insertCoupleFast,onePointMovePD)) exhaust
    new BestSlopeFirst(List(pickupDeliveryCoupleShift,oneCoupleMove,insertCoupleSlow,onePointMovePD, pickupDeliveryCoupleExchange, pickupDeliverySegmentExchange, threeOpt(20,true)))

  val searchWithRrestart = search onExhaustRestartAfter(Atomic(removeCouple maxMoves((n-v)/2)),5,myPDP.obj)

  val removeOnExhaust = removeCouple maxMoves((n-v)/20)

  //TODO: c'est un peu bizarre de faire un reset sur un autre voisinage dans un afterMove.
  val removeMaxCouple = Profile(Atomic(removeOnExhaust) name "remove Some Couples") afterMove(removeOnExhaust reset())

  val search4 = BestSlopeFirst(List(insertCoupleFast,onePointMovePD),refresh = 5) exhaust
    (BestSlopeFirst(List(insertCoupleSlow,
      oneCoupleMove,onePointMovePD,
      treeopt,pickupDeliveryCoupleExchange,
      smartInsertCoupleNotFair, insertCoupleNotFair),
      refresh = 10)
      exhaust pickupDeliverySegmentExchange
      onExhaustRestartAfter (removeMaxCouple,2,myPDP.obj))


  search4.verbose = 2

  search4.doAllMoves(obj=myPDP.obj)

  println(myPDP)
  println(search4.profilingStatistics)

}
