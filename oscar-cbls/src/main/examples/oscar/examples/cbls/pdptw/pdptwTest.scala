package oscar.examples.cbls.pdptw

import java.awt.Toolkit
import javax.swing.JFrame

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.invariants.lib.numeric.{Abs, Sum}
import oscar.cbls.routing.model._
import oscar.cbls.routing.neighborhood._
import oscar.cbls.search.StopWatch
import oscar.cbls.search.combinators._
import oscar.examples.cbls.routing.RoutingMatrixGenerator
import oscar.examples.cbls.routing.visual.ColorGenerator
import oscar.examples.cbls.routing.visual.MatrixMap.{PickupAndDeliveryMatrixVisualWithAttribute, PickupAndDeliveryPoints, RoutingMatrixVisualWithAttribute, RoutingMatrixVisual}
import oscar.visual.VisualFrame

/**
  * *****************************************************************************
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
  * ****************************************************************************
  */

/**
  * @author fabian.germeau@student.vinci.be
  */

class MyPDPTWVRP(n:Int, v:Int, model:Store, distanceMatrix: Array[Array[Int]],unroutedPenalty:Int)
  extends VRP(n,v,model)
    with HopDistanceAsObjectiveTerm
    with HopClosestNeighbors
    with PositionInRouteAndRouteNr
    with NodesOfVehicle
    with PenaltyForUnrouted
    with PenaltyForUnroutedAsObjectiveTerm
    with PenaltyForEmptyRoute
    with PenaltyForEmptyRouteAsObjectiveTerm
    with hopDistancePerVehicle
    with VehicleWithCapacity
    with PickupAndDeliveryCustomersWithTimeWindow{

  installCostMatrix(distanceMatrix)
  setUnroutedPenaltyWeight(unroutedPenalty)
  setEmptyRoutePenaltyWeight(unroutedPenalty)
  closeUnroutedPenaltyWeight()
  computeClosestNeighbors()
  println("end compute closest, install matrix")
  installHopDistancePerVehicle()
  println("end install matrix, posting constraints")
  addRandomPickupDeliveryCouples()
  setArrivalLeaveLoadValue()
  setVehiclesMaxCargo(4)
  setVehiclesCargoStrongConstraint()

  //evenly spreading the travel among vehicles
  val averageDistanceOnAllVehicles = overallDistance.value / V
  val spread = Sum(hopDistancePerVehicle.map(h => Abs(h.value - averageDistanceOnAllVehicles)))
  addObjectiveTerm(spread)
  addObjectiveTerm(emptyRoutePenalty)

  println("vrp done")
  println(strongConstraints)
}

object pdptwTest extends App with StopWatch {

  this.startWatch()

  val n = 65
  val v = 15

  println("PDPTWTest(n:" + n + " v:" + v + ")")

  val mapSize = 10000

  val (distanceMatrix,positions) = RoutingMatrixGenerator(n,mapSize)
  println("compozed matrix " + getWatch + "ms")


  val model = new Store(noCycle = false)

  val vrp = new MyPDPTWVRP(n,v,model,distanceMatrix,mapSize)

  model.close()


  val f = new JFrame("PDPTWTest - Routing Map")
  f.setSize(Toolkit.getDefaultToolkit.getScreenSize.getWidth.toInt,(11*Toolkit.getDefaultToolkit().getScreenSize().getHeight/12).toInt)
  val rm = new PickupAndDeliveryMatrixVisualWithAttribute("Routing Map",vrp,mapSize,positions.toList,ColorGenerator.generateRandomColors(v),dimension = f.getSize)
  f.add(rm)
  f.pack()
  f.setVisible(true)

  println("closed model " + getWatch + "ms")
  val insertPointRoutedFirst = Profile(InsertPointRoutedFirst(
    insertionPoints = vrp.routed,
    unroutedNodesToInsert = () => vrp.kNearest(10,!vrp.isRouted(_)),
    vrp = vrp) guard(() => vrp.unrouted.value.nonEmpty))

  val insertPointUnroutedFirst = Profile(InsertPointUnroutedFirst(
    unroutedNodesToInsert= vrp.unrouted,
    relevantNeighbors = () => vrp.kNearest(10,vrp.isRouted(_)),
    vrp = vrp))

  val insertPointUnroutedFirstBest = Profile(InsertPointUnroutedFirst(
    unroutedNodesToInsert= vrp.unrouted,
    relevantNeighbors = () => vrp.kNearest(1,vrp.isRouted(_)),
    neighborhoodName = "insertPointUnroutedFirstBest",
    vrp = vrp, best = true))

  val pivot = vrp.N/2

  val compositeInsertPoint = Profile(insertPointRoutedFirst guard (() => vrp.unrouted.value.size >= pivot)
    orElse (insertPointUnroutedFirst guard (() => vrp.unrouted.value.size < pivot)))

  //the other insertion point strategy is less efficient, need to investigate why.
  val insertPoint = compositeInsertPoint //insertPointUnroutedFirstBest //new BestSlopeFirst(List(insertPointUnroutedFirst,insertPointRoutedFirst),refresh = 50) //compositeInsertPoint //insertPointUnroutedFirst

  val onePointMove = Profile(OnePointMove(
    nodesPrecedingNodesToMove = vrp.routed,
    relevantNeighbors= () => vrp.kNearest(50),
    vrp = vrp))

  val twoOpt = Profile(TwoOpt(
    predecesorOfFirstMovedPoint = vrp.routed,
    relevantNeighbors = () => vrp.kNearest(20),
    vrp = vrp))

  val threeOpt = Profile(ThreeOpt(
    potentialInsertionPoints = vrp.routed,
    relevantNeighbors = () => vrp.kNearest(20),
    vrp = vrp,
    skipOnePointMove = true))

  val segExchange = Profile(SegmentExchange(vrp = vrp,
    relevantNeighbors = () => vrp.kNearest(40),
    vehicles=() => vrp.vehicles.toList))

  val insertCouple = Profile(AndThen(
    InsertPointUnroutedFirst(
    unroutedNodesToInsert = () => vrp.getUnroutedPickups,
    relevantNeighbors = () => vrp.kNearest(n,vrp.isADepot),
    vrp = vrp),
    InsertPointUnroutedFirst(
    unroutedNodesToInsert = () => vrp.getUnroutedDeliverys,
    relevantNeighbors = () => vrp.kNearest(n,vrp.isRouted),
    vrp = vrp, best = true)))

  val oneCoupleMove = Profile(DynAndThen(OnePointMove(
    nodesPrecedingNodesToMove = () => vrp.getRoutedPickupsPredecessors,
    relevantNeighbors= () => vrp.kNearest(1000),
    vrp = vrp),
    (onePointMove:OnePointMoveMove) => OnePointMove(
    nodesPrecedingNodesToMove = () => {
      List(vrp.preds(vrp.getRelatedDelivery(onePointMove.movedPoint)).value)
    },
    relevantNeighbors= () => vrp.kNearest(1000),
    vrp = vrp)))

  val onePointMovePD = Profile(new RoundRobin(List(OnePointMove(
    nodesPrecedingNodesToMove = () => vrp.getRoutedPickupsPredecessors,
    relevantNeighbors = () => vrp.getAuthorizedInsertionPositionForPickup(),
    vrp = vrp),OnePointMove(
    nodesPrecedingNodesToMove = () => vrp.getRoutedDeliverysPredecessors,
    relevantNeighbors = () => vrp.getAuthorizedInsertionPositionForDelivery(),
    vrp = vrp))))

  val segExchangePD = Profile(SegmentExchangePickupAndDelivery(vrp = vrp))

  val orOpt = Profile(OrOpt(vrp = vrp))

  val search = new RoundRobin(List(insertCouple,oneCoupleMove, orOpt),1) exhaust
    new BestSlopeFirst(List(onePointMovePD, threeOpt, orOpt, segExchangePD, oneCoupleMove), refresh = n / 2) showObjectiveFunction
    vrp.getObjective() afterMove {
    rm.drawRoutes()
    println(vrp.objectiveFunction)
  } // exhaust onePointMove exhaust segExchange//threeOpt //(new BestSlopeFirst(List(onePointMove,twoOpt,threeOpt)))

  search.verbose = 1
  //    search.verboseWithExtraInfo(3,() => vrp.toString)
  //segExchange.verbose = 3

  def launchSearch(): Unit ={
    //search.verboseWithExtraInfo(1,vrp.toString)
    search.doAllMoves(_ > 10*n, vrp.getObjective())

    println("total time " + getWatch + "ms or  " + getWatchString)

    println("\nresult:\n" + vrp)

    println(search.profilingStatistics)
  }

  launchSearch()
}
