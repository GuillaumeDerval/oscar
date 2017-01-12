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

import java.awt.{Color, Dimension}
import javax.swing.SwingUtilities

import oscar.cbls.core.computation.Store
import oscar.cbls.lib.invariant.logic.Int2Int
import oscar.cbls.lib.invariant.minmax.{Max, Min}
import oscar.cbls.lib.invariant.numeric.{Abs, Sum}
import oscar.cbls.business.routing.legacy.model._
import oscar.cbls.business.routing.legacy.neighborhood._
import oscar.cbls.lib.search.combinators.{Atomic, RoundRobin, Profile, BestSlopeFirst}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.util.StopWatch
import scala.language.implicitConversions

/**
 * Created by rdl on 15-12-15.
 */


class MyVRP(n:Int, v:Int, model:Store, distanceMatrix: Array[Array[Int]],unroutedPenaltyWeight:Int, loadBalancing:Double = 1.0)
  extends VRP(n,v,model)
  with HopDistanceAsObjectiveTerm
  with HopClosestNeighbors
  with PositionInRouteAndRouteNr
  with NodesOfVehicle
  with PenaltyForUnrouted
  with hopDistancePerVehicle
with PenaltyForEmptyRouteAsObjectiveTerm{ //just for the fun of it

  installCostMatrix(distanceMatrix)
  setUnroutedPenaltyWeight(unroutedPenaltyWeight)
  closeUnroutedPenaltyWeight()
  computeClosestNeighbors()
  println("end compute closest, install matrix")
  installHopDistancePerVehicle()
  println("end install matrix, posting constraints")

  //evenly spreading the travel among vehicles
  val averageDistanceOnAllVehicles = overallDistance / V
  val spread = Sum(hopDistancePerVehicle.map(h => Abs(h - averageDistanceOnAllVehicles)))
  val weightedSpread = new Int2Int(spread,(spreadValue:Int) => (spreadValue * loadBalancing).toInt)

  addObjectiveTerm(weightedSpread)
  addObjectiveTerm(unroutedPenalty)

  setEmptyRoutePenaltyWeight(100)
  println("vrp done")
}

object RoutingTest extends App with StopWatch{

  this.startWatch()

  val n = 200
  val v = 5

  println("RoutingTest(n:" + n + " v:" + v + ")")

  val (distanceMatrix,positions) = RoutingMatrixGenerator(n,10000)

  println("compozed matrix " + getWatch + "ms")

  val model = new Store()

  val vrp = new MyVRP(n,v,model,distanceMatrix,100000)

  model.close()

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

  val search = new RoundRobin(List(insertPoint,onePointMove),10) exhaust
                      new BestSlopeFirst(List(onePointMove, threeOpt, segExchange), refresh = n / 2) showObjectiveFunction
    vrp.getObjective() // exhaust onePointMove exhaust segExchange//threeOpt //(new BestSlopeFirst(List(onePointMove,twoOpt,threeOpt)))

  search.verbose = 1
//    search.verboseWithExtraInfo(3,() => vrp.toString)
  //segExchange.verbose = 3

  def launchSearch(): Unit ={
    search.doAllMoves(_ > 10*n, vrp.objectiveFunction)

    println("total time " + getWatch + "ms or  " + getWatchString)

    println("\nresult:\n" + vrp)

    println(search.profilingStatistics)
  }

  launchSearch()
}
