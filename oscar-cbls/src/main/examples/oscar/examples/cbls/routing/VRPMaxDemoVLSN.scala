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

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.neighborhood.vlsn.VLSN
import oscar.cbls.core.search.{Best, First}
import oscar.cbls.util.StopWatch

//50.404631, 4.452595
//50.415162, 4.440849


object VRPMaxDemoVLSN  extends App {

  println("usage: VRPMaxDemo n v")
  val n:Int=args(0).toInt
  val v = args(1).toInt

  //50.404631, 4.452595
  //50.415162, 4.440849

  val displayDelay = if (n >= 1000) 1500 else 500 //ms
  val verbose = 1
  val maxPivotPerValuePercent = 4
  val mapSide = 1000

  new VRPMaxDemo(n,v,maxPivotPerValuePercent,verbose,displayDelay, mapSide)
}

class VRPMaxDemoVLSN (n:Int, v:Int, maxPivotPerValuePercent:Int, verbose:Int, displayDelay:Int, mapSide:Int) extends StopWatch{

  val (symmetricDistanceMatrix1,nodesPositions) = RoutingMatrixGenerator.geographicRandom(n, minLong,maxLong,minLat,maxLat)

  val symmetricDistanceMatrix = Array.tabulate(n)({a =>
    Array.tabulate(n)({b =>
      symmetricDistanceMatrix1(a min b)(a max b).toInt
    })})

  val maxWorkloadPerVehicle = 2500
  val serviceTimePerNode = 100

  //val maxWorkloadPerVehicle = 4000
  //val serviceTimePerNode = 100

  startWatch()
  val model = new Store()

  val myVRP = new VRP(model,n,v)
  val routeLengthPerVehicle = constantRoutingDistance(myVRP.routes,n,v,perVehicle = true,symmetricDistanceMatrix,true,true,false)
  val totalRouteLength = sum(routeLengthPerVehicle)
  val nodesPerVehicle = nodesOfVehicle(myVRP.routes,v)

  val totalServiceTimePerVehicle = nodesPerVehicle.map(cardinality(_)*serviceTimePerNode)

  val c = new ConstraintSystem(model)

  for(vehicle <- 0 until v){
    val workLoadOfVehicle = totalServiceTimePerVehicle(vehicle) + routeLengthPerVehicle(vehicle)
    c.add(workLoadOfVehicle le maxWorkloadPerVehicle)
  }

  c.close()

  val penaltyForUnrouted  = 10000

  val obj = new CascadingObjective(
    c,
    Objective(totalRouteLength + (penaltyForUnrouted*(n - length(myVRP.routes))))
  )

  model.close()

  val relevantPredecessorsOfNodes = (node:Int) => myVRP.nodes
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistanceMatrix,relevantPredecessorsOfNodes))

  val routedPostFilter = (node:Int) => (neighbor:Int) => myVRP.isRouted(neighbor)
  val unRoutedPostFilter = (node:Int) => (neighbor:Int) => !myVRP.isRouted(neighbor)

  def routeUnroutedPoint(unroutedNodeToInsert:Int,targetVehicle:Int) = {
    val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)
    insertPointUnroutedFirst(
      () => List(unroutedNodeToInsert),
      () => _ => nodesOfTargetVehicle, myVRP, "Insert", false, Best(), Best()
    )
  }

  def movePoint(node:Int,targetVehicle:Int) = {
    val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)
    onePointMove(
      () => List(node),
      () => _ => nodesOfTargetVehicle,
      myVRP,
      selectDestinationBehavior = Best())
  }

  val routeUnroutedPoint =  profile(insertPointUnroutedFirst(myVRP.unrouted,
    ()=>myVRP.kFirst(10,closestRelevantNeighborsByDistance,routedPostFilter),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false,
    selectNodeBehavior = First(),
    selectInsertionPointBehavior = Best()))


  def onePtMove(k:Int) = profile(onePointMove(
    myVRP.routed,
    () => myVRP.kFirst(k,closestRelevantNeighborsByDistance,routedPostFilter),
    myVRP,
    selectDestinationBehavior = Best()))

  def customTwoOpt(k:Int=10) = profile(twoOpt(myVRP.routed, ()=>myVRP.kFirst(k,closestRelevantNeighborsByDistance,routedPostFilter), myVRP))

  def customThreeOpt(k:Int, breakSym:Boolean) =
    profile(threeOpt(myVRP.routed, ()=>myVRP.kFirst(k,closestRelevantNeighborsByDistance,routedPostFilter), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))


  val search = (bestSlopeFirst(List(
    routeUnroutedPoint,
    onePtMove(10),
    customTwoOpt(20),
    customThreeOpt(10,true)))
    exhaust new VLSN(v,

    vehicleToRoutedNodesToMove:() => SortedMap[Int,Iterable[Int]],
    unroutedNodesToInsert:() => Iterable[Int],
    nodeToRelevantVehicles:() => Map[Int,Iterable[Int]],

    nodeVehicleToInsertNeighborhood:(Int,Int) => Neighborhood with SupportForAndThenChaining[PotentiallyComposableMove],
    nodeTargetVehicleToMoveNeighborhood:(Int,Int) => Neighborhood with SupportForAndThenChaining[PotentiallyComposableMove],
    nodeToRemoveNeighborhood:Int => Neighborhood with SupportForAndThenChaining[PotentiallyComposableMove],
    removeNodeAndReInsert:Int => () => Unit,


    search.verbose = verbose
      //search.verboseWithExtraInfo(1, ()=> "" + myVRP)
      //  routeUnroutdPoint.verbose= 4
      search.doAllMoves(obj = obj)

    graphicExtension.drawRoutes()
  print(myVRP)
}
