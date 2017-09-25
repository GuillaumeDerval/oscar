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

import oscar.cbls.benchmarks.CP2017.RoutingMatrixGenerator
import oscar.cbls.business.routing.model._
import oscar.cbls.business.routing.model.extensions.{Display, Distance}
import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.core.computation.Store
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.propagation.ErrorChecker
import oscar.cbls.core.search.{Best, First}
import oscar.cbls.lib.invariant.routing.ConstantRoutingDistance
import oscar.cbls.lib.invariant.seq.Length
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Mu, Profile}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.util.StopWatch

import scala.collection.immutable.SortedSet


object TSPDemo extends App {

  val n = 1000
  val v = 10
  val displayDelay = 500 //ms
  val verbose = 1
  val maxPivotPerValuePercent = 3
  val mapSide = 1000

  new TSPDemo(n,v,maxPivotPerValuePercent,verbose,displayDelay, mapSide)
}

class TSPDemo(n:Int,v:Int,maxPivotPerValuePercent:Int, verbose:Int, displayDelay:Int, mapSide:Int) extends StopWatch{

  val routingMatrix = RoutingMatrixGenerator(n,side=mapSide)
  val symmetricDistanceMatrix = routingMatrix._1
  val nodesPositions = routingMatrix._2

  startWatch()
  val model = new Store()

  val myVRP = new VRP(model,n,v)
  val constantRoutingDistance = ConstantRoutingDistance(myVRP.routes,n,v,false,symmetricDistanceMatrix,true,true,false)
  val distanceExtension = new Distance(myVRP,symmetricDistanceMatrix,constantRoutingDistance)
  val graphicExtension = new Display(myVRP,nodesPositions.map(np => (np._1.toDouble,np._2.toDouble)).toList,sizeOfMap = Some(mapSide), refreshRate = displayDelay)
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(distanceExtension.computeClosestPathFromNeighbor(myVRP.preComputedRelevantNeighborsOfNodes))

  val penaltyForUnrouted  = 10000

  val obj = Objective(distanceExtension.totalDistance + (penaltyForUnrouted*(n - Length(myVRP.routes))))


  model.close()

  val routeUnroutedPoint =  Profile(InsertPointUnroutedFirst(myVRP.unrouted,
    ()=>myVRP.kFirst(10,closestRelevantNeighborsByDistance,myVRP.isRouted),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false,
    selectNodeBehavior = First(),
    selectInsertionPointBehavior = Best()))

  val routeUnroutedPointBad =  Profile(InsertPointUnroutedFirst(myVRP.unrouted,
    ()=> myVRP.kFirst(20,closestRelevantNeighborsByDistance,myVRP.isRouted),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false))


  //using post-filters on k-nearest is probably a bit slower than possible for large problems.
  //that's why we prefer to block this neighborhood when many nodes are already routed (so few are unrouted, so the filter filters many nodes away)
  val routeUnroutedPoint2 =  Profile(InsertPointRoutedFirst(
    myVRP.routed,
    ()=>myVRP.kFirst(10,closestRelevantNeighborsByDistance,x => !myVRP.isRouted(x)),  //should be the backward ones but this is a symmetric distance so we do not care
    myVRP,
    neighborhoodName = "InsertRF")
    guard(() => myVRP.routed.value.size < n/2))

  def onePtMove(k:Int) = Profile(OnePointMove(
    myVRP.routed,
    () => myVRP.kFirst(k,closestRelevantNeighborsByDistance,myVRP.isRouted),
    myVRP,
    selectDestinationBehavior = Best()))

  val twoOpt = Profile(TwoOpt(myVRP.routed, ()=>myVRP.kFirst(20,closestRelevantNeighborsByDistance,myVRP.isRouted), myVRP))

  def threeOpt(k:Int, breakSym:Boolean) =
    Profile(ThreeOpt(myVRP.routed, ()=>myVRP.kFirst(k,closestRelevantNeighborsByDistance,myVRP.isRouted), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val vlsn1pt = Mu[OnePointMoveMove](
    OnePointMove(myVRP.routed, () => myVRP.kFirst(5,closestRelevantNeighborsByDistance,myVRP.isRouted),myVRP),
    l => Some(OnePointMove(() => List(l.head.newPredecessor).filter(_ >= v), () => myVRP.kFirst(3,closestRelevantNeighborsByDistance,myVRP.isRouted),myVRP, hotRestart = false)),
    intermediaryStops = true,
    maxDepth = 6)

  def segExchange(k:Int) = SegmentExchange(myVRP,()=>myVRP.kFirst(k,closestRelevantNeighborsByDistance,myVRP.isRouted),() => myVRP.vehicles)

  val search = (BestSlopeFirst(List(routeUnroutedPoint, routeUnroutedPoint2, vlsn1pt, onePtMove(10),twoOpt, threeOpt(10,true),segExchange(10))) exhaust BestSlopeFirst(List(threeOpt(30,true),vlsn1pt))).afterMove(
    graphicExtension.drawRoutes()) //showObjectiveFunction(myVRP.obj)

  search.verbose = verbose
  //search.verboseWithExtraInfo(1, ()=> "" + myVRP)
//  routeUnroutdPoint.verbose= 4
  search.doAllMoves(obj = obj)

  graphicExtension.drawRoutes()
  print(myVRP)
}
