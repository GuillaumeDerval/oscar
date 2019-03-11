package oscar.examples.cbls.routing.drone

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
import oscar.cbls.business.routing.neighborhood.vlsn.{CycleFinderAlgoType, VLSN}
import oscar.cbls.core.search.{Best, First, Neighborhood, NoMoveNeighborhood}
import oscar.examples.cbls.routing.RoutingMatrixGenerator

import scala.collection.immutable.{SortedMap, SortedSet}

object VRPAutonomyDemo extends App {

  val n = 200L
  val v = 10L

  val vehicles = 0L until v

  val displayDelay = if (n >= 1000) 1500 else 500 //ms
  val verbose = 1

  val minLat = 50.404631
  val maxLat = 50.415162
  val minLong = 4.440849
  val maxLong = 4.452595

  val (symmetricDistanceMatrix1,nodesPositions) = RoutingMatrixGenerator(n)
  //val (symmetricDistanceMatrix1,nodesPositions) = RoutingMatrixGenerator.geographicRandom(n, minLong,maxLong,minLat,maxLat) //for gpx file

  val symmetricDistanceMatrix = Array.tabulate(n)({a =>
    Array.tabulate(n)({b =>
      symmetricDistanceMatrix1(a min b)(a max b)
    })})

  val droppedWeights = Array.tabulate(n)(dropPerNode => 50L)

  val droneWeight = 1250 //dg
  val maxDroneWeight = 2250 //dg

  val hoveringPowerComsumption = 3250 //W
  val hoverintTime = 10 * 60 //seconde
  //val batteryCapacity: Int = hoveringPowerComsumption * hoverintTime //J
  val batteryCapacity: Long = 400000000000L //J


  /**
    * a,b,c constant of the drone (MatLab script)
    * d coefficient convert to microJ to J
    */
  val a: Long = 8
  val b: Long = -9504
  val c: Long = 271590000

  val model = new Store()

  val myVRP = new VRP(model,n,v)
  val routeLengthPerVehicle = routeLength(myVRP.routes,n,v,perVehicle = true,symmetricDistanceMatrix,true,true,false)
  val totalRouteLength = sum(routeLengthPerVehicle)
  val nodesPerVehicle = nodesOfVehicle(myVRP.routes,v)

  val routeEnergyPerVehicle: Array[CBLSIntVar] = Array.tabulate[CBLSIntVar](v)(vehicle => CBLSIntVar(model,name="energyOfVehicul" + vehicle))
  val routeWeightPerVehicle: Array[CBLSIntVar] = Array.tabulate[CBLSIntVar](v)(vehicle => CBLSIntVar(model,name="weightOfVehicle" + vehicle))

  val totalEnergy = sum(routeEnergyPerVehicle)
  val totalWeight = sum(routeWeightPerVehicle)

  val auto = new AutonomyGlobalConstraint(a,b,c,v,symmetricDistanceMatrix,droppedWeights,droneWeight,routeEnergyPerVehicle,routeWeightPerVehicle,myVRP.routes)

  val constraint = new ConstraintSystem(model)

  val vehicleToEnergyConstraint = Array.tabulate(v)(
    vehicle => {
      val constr = routeEnergyPerVehicle(vehicle) le batteryCapacity
      constraint.add(constr)
      constr
    }
  )

  val vehicleToWeightConstraint = Array.tabulate(v)(
    vehicle => {
      val constr = routeWeightPerVehicle(vehicle) le maxDroneWeight
      constraint.add(constr)
      constr
    }
  )

  constraint.close()

  val penaltyForUnrouted  = 1000000000000L

  val graphicExtension = display(myVRP,nodesPositions.map(np => (np._2.toDouble,np._1.toDouble)),sizeOfMap = None, refreshRate = displayDelay)

  val unroutedPenaltyObj = Objective(penaltyForUnrouted*(n - length(myVRP.routes)))

  val obj = new CascadingObjective(
    constraint,
    Objective(totalEnergy + unroutedPenaltyObj.objective)
  )

  val objPerVehicle = Array.tabulate[Objective](v)(vehicle =>
    new CascadingObjective(
      Objective(vehicleToEnergyConstraint(vehicle).violation + vehicleToWeightConstraint(vehicle).violation),
      Objective(routeEnergyPerVehicle(vehicle)))
  )

  model.close()

  def printVRP(): Unit = {
    println(myVRP)
    for(vehicle <- 0 until v){
      println("energy needed for route " + vehicle + " : " + routeEnergyPerVehicle(vehicle).value + "/"+batteryCapacity + " J"
        + " and " + routeWeightPerVehicle(vehicle).value + "0/"+maxDroneWeight + "0 g")

    }
  }

  val relevantPredecessorsOfNodes = (node:Long) => myVRP.nodes
  val closestRelevantNeighborsByDistance = DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistanceMatrix,relevantPredecessorsOfNodes)(_)

  val routedPostFilter = (node:Long) => (neighbor:Long) => myVRP.isRouted(neighbor)

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

  val customTwoOpt = profile(twoOpt(myVRP.routed, ()=>myVRP.kFirst(40,closestRelevantNeighborsByDistance,routedPostFilter), myVRP))

  def customThreeOpt = threeOpt(myVRP.routed,() => _ => myVRP.routed.value,myVRP)

  def vlsn(l:Int = Int.MaxValue) = {

    val lClosestNeighborsByDistance: Array[SortedSet[Long]] = Array.tabulate(n)(node =>
      SortedSet.empty[Long] ++ myVRP.kFirst(l, closestRelevantNeighborsByDistance)(node))

    //VLSN neighborhood
    val nodeToAllVehicles = SortedMap.empty[Long, Iterable[Long]] ++ (v until n).map(node => (node, vehicles))

    def routeUnroutedPointVLSN(targetVehicle: Long):Long => Neighborhood = {
      val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)

      unroutedNodeToInsert:Long => {
        val lNearestNodesOfTargetVehicle = nodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(unroutedNodeToInsert) contains x)
        insertPointUnroutedFirst(
          () => List(unroutedNodeToInsert),
          () => _ => lNearestNodesOfTargetVehicle,
          myVRP,
          hotRestart = false,
          selectInsertionPointBehavior = Best(),
          positionIndependentMoves = true //compulsory because we are in VLSN
        )
      }
    }

    //targetVehicleNodeToMoveNeighborhood:Int => Int => Neighborhood,
    def movePointVLSN(targetVehicle: Long): Long => Neighborhood = {
      val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)

      nodeToMove:Long => {
        val lNearestNodesOfTargetVehicle = nodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(nodeToMove) contains x)
        onePointMove(
          () => List(nodeToMove),
          () => _ => lNearestNodesOfTargetVehicle,
          myVRP,
          selectDestinationBehavior = Best(),
          hotRestart = false,
          positionIndependentMoves = true  //compulsory because we are in VLSN
        )
      }
    }

    def removePointVLSN(node: Long) =
      removePoint(
        () => List(node),
        myVRP,
        positionIndependentMoves = true,
        hotRestart = false)

    //for re-optimization
    def threeOptOnVehicle(vehicle:Int) = {
      val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(vehicle)
      //insertions points are position where we perform the insert,
      // basically the segment will start in place of the insertion point and the insertion point will be moved upward
      val nodesOfTargetVehicleButVehicle = nodesOfTargetVehicle.filter(_ != vehicle)

      threeOpt(() => nodesOfTargetVehicle,
        () => _ => nodesOfTargetVehicleButVehicle,
        myVRP,
        breakSymmetry = false).afterMove(graphicExtension.drawRoutes())
    }

    def removeAndReInsertVLSN(pointToRemove: Long): (() => Unit) = {
      val checkpointBeforeRemove = myVRP.routes.defineCurrentValueAsCheckpoint(true)
      require(pointToRemove >= v, "cannot remove vehicle point: " + v)

      myVRP.routes.value.positionOfAnyOccurrence(pointToRemove) match {
        case None => throw new Error("cannot remove non routed point:" + pointToRemove)
        case Some(positionOfPointToRemove) =>
          myVRP.routes.remove(positionOfPointToRemove)
      }

      def restoreAndRelease: () => Unit = () => {
        myVRP.routes.rollbackToTopCheckpoint(checkpointBeforeRemove)
        myVRP.routes.releaseTopCheckpoint()
      }

      restoreAndRelease
    }

    new VLSN(
      v,
      () => SortedMap.empty[Long, SortedSet[Long]] ++
        vehicles.map((vehicle: Long) =>
          (vehicle, (SortedSet.empty[Long] ++ myVRP.getRouteOfVehicle(vehicle).filter(_ >= v)))),
      () => SortedSet.empty[Long] ++ myVRP.unroutedNodes,
      nodeToRelevantVehicles = () => nodeToAllVehicles,

      targetVehicleNodeToInsertNeighborhood = routeUnroutedPointVLSN,
      targetVehicleNodeToMoveNeighborhood = movePointVLSN,
      removePointVLSN,

      removeNodeAndReInsert = removeAndReInsertVLSN,

      reOptimizeVehicle = Some(vehicle => Some(threeOptOnVehicle(vehicle))),
      useDirectInsert = false,

      objPerVehicle,
      unroutedPenaltyObj,
      obj,

      cycleFinderAlgoSelection = CycleFinderAlgoType.Mouthuy,

      name="VLSN(" + l + ")"
    )
  }

  val search = ((bestSlopeFirst(List(
    routeUnroutedPoint,
    onePtMove(10),
    customTwoOpt)))
    exhaust profile(vlsn(40) maxMoves 1)).afterMove(
    graphicExtension.drawRoutes())

  /*
  val search = (bestSlopeFirst(List(
    routeUnroutedPoint,
    onePtMove(10),
    customTwoOpt,
    customThreeOpt andThen customThreeOpt))).afterMove(
    graphicExtension.drawRoutes())
   */
  search.verbose = 3
  search.doAllMoves(obj = obj)
  graphicExtension.drawRoutes(force = true)

  //val GPX = new GPXGeneration(v,myVRP.routes.value,nodesPositions)
  //GPX.writeAndSave();

  printVRP()
  println(search.profilingStatistics)
}