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

import scala.collection.immutable.SortedMap

//50.404631, 4.452595
//50.415162, 4.440849


object VRPMaxDemoVLSN  extends App {

  println("usage: VRPMaxDemo n v")
  val n:Int=args(0).toInt
  val v = args(1).toInt

  val displayDelay = if (n >= 1000) 1500 else 500 //ms
  val verbose = 1
  val maxPivotPerValuePercent = 4
  val mapSide = 1000

  new VRPMaxDemo(n,v,maxPivotPerValuePercent,verbose,displayDelay, mapSide)
}

class VRPMaxDemoVLSN (n:Int, v:Int, maxPivotPerValuePercent:Int, verbose:Int, displayDelay:Int, mapSide:Int) extends StopWatch{

  val (symmetricDistanceMatrix1,nodesPositions) = RoutingMatrixGenerator.apply(n,1000)

  val symmetricDistanceMatrix = Array.tabulate(n)({a =>
    Array.tabulate(n)({b =>
      symmetricDistanceMatrix1(a min b)(a max b).toInt
    })})

  val maxWorkloadPerVehicle = 2500
  val serviceTimePerNode = 100
  val vehicles = 0 until v

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

  val vehicletoWorkload = Array.tabulate(v)(
    vehicle => totalServiceTimePerVehicle(vehicle) + routeLengthPerVehicle(vehicle)
  )

  val vehicleToWorkloadConsraint = Array.tabulate(v)(
    vehicle => {
      val constr = vehicletoWorkload(vehicle) le maxWorkloadPerVehicle
      c.add(constr)
      constr
    }
  )

  c.close()

  val penaltyForUnrouted  = 10000


  val objPerVehicle = Array.tabulate[Objective](v)(vehicle =>
    new CascadingObjective(
      Objective(vehicleToWorkloadConsraint(vehicle).violation),
      Objective(vehicletoWorkload(vehicle)))
  )

  val unroutedPenaltyObj = Objective(penaltyForUnrouted*(n - length(myVRP.routes)))

  val obj = new CascadingObjective(
    c,
    Objective(totalRouteLength + unroutedPenaltyObj.objective)
  )

  model.close()

  val relevantPredecessorsOfNodes = (node:Int) => myVRP.nodes
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistanceMatrix,relevantPredecessorsOfNodes))

  val routedPostFilter = (node:Int) => (neighbor:Int) => myVRP.isRouted(neighbor)
  val unRoutedPostFilter = (node:Int) => (neighbor:Int) => !myVRP.isRouted(neighbor)


  //VLSN neighborhood
  val nodeToAllVehicles = SortedMap.empty[Int,Iterable[Int]] ++ (v until n).map(node => (node,vehicles))

  def routeUnroutedPointVLSN(unroutedNodeToInsert:Int,targetVehicle:Int) = {
    val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)
    insertPointUnroutedFirst(
      () => List(unroutedNodeToInsert),
      () => _ => nodesOfTargetVehicle,
      myVRP,
      hotRestart = false,
      selectInsertionPointBehavior = Best(),
      positionIndependentMoves = true //combulsory because e are in VLSN mode!!!
    )
  }

  def movePointVLSN(node:Int,targetVehicle:Int) = {
    val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)
    onePointMove(
      () => List(node),
      () => _ => nodesOfTargetVehicle,
      myVRP,
      selectDestinationBehavior = Best(),
      hotRestart = false)
  }

  def removePointVLSN(node:Int) =
    removePoint(
      () => List(node),
      myVRP,
      hotRestart = false)

  def removeAndReInsertVLSN(pointToRemove:Int):(() => Unit) = {
    val checkpointBeforeRemove = myVRP.routes.defineCurrentValueAsCheckpoint(true)
    require(pointToRemove > v, "cannot remove vehicle point: " + v)

    myVRP.routes.value.positionOfAnyOccurrence(pointToRemove) match {
      case None => throw new Error("cannot remove non routed point:" + pointToRemove)
      case Some(positionOfPointToRemove) =>
        myVRP.routes.remove(positionOfPointToRemove)
    }

    def restoreAndRelease():Unit = {
      myVRP.routes.rollbackToTopCheckpoint(checkpointBeforeRemove)
      myVRP.routes.releaseTopCheckpoint()
    }
    restoreAndRelease
  }

  def vlsn = new VLSN(
    v,
    vehicleToRoutedNodesToMove = () => SortedMap.empty[Int,List[Int]] ++ vehicles.map((v:Int) => (v,myVRP.getRouteOfVehicle(v))),

    unroutedNodesToInsert = () => myVRP.unroutedNodes,
    nodeToRelevantVehicles = () => nodeToAllVehicles,

    nodeVehicleToInsertNeighborhood = routeUnroutedPointVLSN,
    nodeTargetVehicleToMoveNeighborhood = movePointVLSN,
    removePointVLSN,
    removeNodeAndReInsert = removeAndReInsertVLSN,

    objPerVehicle,
    unroutedPenaltyObj
  )




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
    exhaust vlsn)

  search.verbose = verbose
  //search.verboseWithExtraInfo(1, ()=> "" + myVRP)
  //  routeUnroutdPoint.verbose= 4
  search.doAllMoves(obj = obj)


  print(myVRP)
}
