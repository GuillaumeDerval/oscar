package oscar.examples.cbls.routing

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

import oscar.cbls.core.computation.Store
import oscar.cbls.business.routing.legacy.model._
import oscar.cbls.business.routing.legacy.neighborhood._
import oscar.cbls.lib.search.combinators.{Profile, BestSlopeFirst}
import oscar.cbls.util.StopWatch

class MyVRPHS(n:Int, v:Int, model:Store, distanceMatrix: Array[Array[Int]],unroutedPenaltyWeight:Int)
  extends VRP(n,v,model)
  with HopDistanceAsObjectiveTerm
  with HopClosestNeighbors
  with PositionInRouteAndRouteNr
  with NodesOfVehicle
  with PenaltyForUnroutedAsObjectiveTerm
  with HotSpot {
  installCostMatrix(distanceMatrix)
  setUnroutedPenaltyWeight(unroutedPenaltyWeight)
  computeClosestNeighbors()
  closeUnroutedPenaltyWeight()
}

object RoutingTestHotSpot extends App with StopWatch{

  this.startWatch()

  val n = 500
  val v = 5

  println("RoutingTest(n:" + n + " v:" + v + ")")

  val (distanceMatrix,positions) = RoutingMatrixGenerator(n,1000)

  println("compozed matrix " + getWatch + "ms")

  val model = new Store()

  val vrp = new MyVRPHS(n,v,model,distanceMatrix,1000)

  println("closing model")
  model.close()

  vrp.makeEverythingHot()

  println("closed model " + getWatch + "ms")

  val insertPointRoutedFirst = new InsertPointRoutedFirst(
    insertionPoints = vrp.routed,
    unroutedNodesToInsert = () => vrp.kNearest(10,!vrp.isRouted(_)),
    vrp = vrp)

  val insertPointUnroutedFirst = Profile(InsertPointUnroutedFirst(
    unroutedNodesToInsert = () => vrp.unrouted.value,
    relevantNeighbors = () => vrp.kNearest(20,vrp.isRouted(_)),
    vrp = vrp, hotRestart = false))

  //the other insertion point strategy is less efficient, need to investigate why.
  val insertPoint = insertPointUnroutedFirst

  val onePointMove = Profile(OnePointMove(
    nodesPrecedingNodesToMove = vrp.routed,
    relevantNeighbors= () => vrp.kNearest(20),
    vrp = vrp))

  //the one point move with HotSpot. notice that it is smalle than without hotspot because the pirmary must be hot
  //however, there could be move with primary cold and secondary hot; these are not explored (in the todo-list)
  val onePointHotView = vrp.newVehicleHotSpotView(true)
  val onePointHot = Profile(OnePointMove(
    nodesPrecedingNodesToMove = () => onePointHotView.hotNodesByVehicleWithConsumption,
    relevantNeighbors= () => vrp.kNearest(20),
    vrp = vrp,
    neighborhoodName = "OnePointMoveHot",
    hotRestart = false))

  val onePointHotView2 = vrp.newNodeHotSpotView(true)
  val onePointHot2 = Profile(OnePointMove(
    nodesPrecedingNodesToMove = () => onePointHotView2.hotNodesWithConsumption,
    relevantNeighbors = () => vrp.kNearest(20),
    vrp = vrp,
    neighborhoodName = "OnePointMoveHot",
    hotRestart = false))

  val twoOpt = Profile(TwoOpt(
    predecesorOfFirstMovedPoint = vrp.routed,
    relevantNeighbors = () => vrp.kNearest(20),
    vrp = vrp))

  val twoOptMoveHotView = vrp.newVehicleHotSpotView(true)
  val twoOptHot = Profile(TwoOpt(
    predecesorOfFirstMovedPoint = () => twoOptMoveHotView.hotNodesByVehicleWithConsumption,
    relevantNeighbors = () => vrp.kNearest(20),
    vrp = vrp,
    neighborhoodName = "TwoOptHot",
    hotRestart = false))

  val twoOptMoveHotView2 = vrp.newNodeHotSpotView(true)
  val twoOptHot2 = Profile(TwoOpt(
    predecesorOfFirstMovedPoint = () => twoOptMoveHotView2.hotNodesWithConsumption,
    relevantNeighbors = () => vrp.kNearest(20),
    vrp = vrp,
    neighborhoodName = "TwoOptHot",
    hotRestart = false))

  val threeOpt = Profile(ThreeOpt(
    potentialInsertionPoints = vrp.routed,
    relevantNeighbors = () => vrp.kNearest(20),
    vrp = vrp, skipOnePointMove = true))


  val searchHot = () => {vrp.makeEverythingHot();("hotVehicles",(insertPoint exhaust (new BestSlopeFirst(List(onePointHot,twoOptHot,threeOpt),refresh = 10000))) afterMoveOnMove(vrp.updateHotSpotAfterMoveAnyMove(_)))}


  val searchHot2 = () => {vrp.makeEverythingHot();("hotNodes",(insertPoint exhaust (new BestSlopeFirst(List(onePointHot2,threeOpt),refresh = n/10))) afterMoveOnMove(vrp.updateHotSpotAfterMoveAnyMove(_)))}


  val search = () => ("standard",(insertPoint exhaust (new BestSlopeFirst(List(onePointMove,twoOpt,threeOpt),refresh = 10000))))


  val searchNoRefresh = () => ("standard no refresh",(insertPoint exhaust (new BestSlopeFirst(List(onePointMove,twoOpt,threeOpt),refresh = 10000))))
  val search3 = () => ("3-opt",(insertPoint exhaust threeOpt))
  val search31 = () => ("3-opt AT 1-opt",(insertPoint exhaust threeOpt exhaust onePointMove))
  //  search.verbose = 1
  //  search.verboseWithExtraInfo(3,() => vrp.toString)

  //  onePointMove.verboseWithExtraInfo(3,() => vrp.toString)



  val searchh =  searchHot2()._2 //(insertPoint exhaust (onePointHot2 exhaustBack threeOpt). afterMove(vrp.updateHotSpotAfterMoveAnyMove(_)))

  searchh.verbose = 1
  searchh.doAllMoves(_ > 10*n, vrp.objectiveFunction)
  println(searchh.profilingStatistics)
/*

  insertPointUnroutedFirst.doAllMoves(_ > 10*n, vrp.objectiveFunction)

  println(Benchmark.benchToStringFull(vrp.objectiveFunction,1,
    List(
      search,
      searchHot,
      searchHot2,
      () => ("OnePoint", onePointMove),
     // () => {vrp.makeEverythingHot(); ("OnePointHotVehicles",onePointHot afterMove(vrp.updateHotSpotAfterMoveAnyMove(_)))},
      () => {vrp.makeEverythingHot(); ("OnePointHotNodes",onePointHot2 afterMove(vrp.updateHotSpotAfterMoveAnyMove(_)))})))

  println("total time " + getWatch + "ms or  " + getWatchString)

  //println(search.profilingStatistics)

 */
    println("\nresult:\n" + vrp)
}
