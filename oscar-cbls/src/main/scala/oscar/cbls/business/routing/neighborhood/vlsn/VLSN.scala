package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.Objective
import oscar.cbls.business.routing.neighborhood.vlsn.CycleFinderAlgoType.CycleFinderAlgoType
import oscar.cbls.core.search._

import scala.collection.immutable.{SortedMap, SortedSet}
import oscar.cbls.business.routing.neighborhood.vlsn.VLSNMoveType._


/**
  * Very Large Scale Neighborhood
  * it searches for a composition of atomic moves that, together improve the objective function, although separatedly, they are not feasible.
  * check @Article{Mouthuy2012,
  *     author="Mouthuy, S{\'e}bastien and Hentenryck, Pascal Van and Deville, Yves",
  *     title="Constraint-based Very Large-Scale Neighborhood search",
  *     journal="Constraints",
  *     year="2012",
  *     month="Apr",
  *     volume="17",
  *     number="2",
  *     pages="87--122"}
  * {{{
  *def vlsn(l:Int = Int.MaxValue) = {
  *
  * val lClosestNeighborsByDistance: Array[SortedSet[Int]] = Array.tabulate(n)(node =>
  *   SortedSet.empty[Int] ++ myVRP.kFirst(l, closestRelevantNeighborsByDistance)(node))
  *
  * val nodeToAllVehicles = SortedMap.empty[Int, Iterable[Int]] ++ (v until n).map(node => (node, vehicles))
  *
  * def routeUnroutedPointVLSN(targetVehicle: Int):(Int => Neighborhood) = {
  *   if(vehicletoWorkload(targetVehicle).value + serviceTimePerNode > maxWorkloadPerVehicle){
  *     (_ => NoMoveNeighborhood)
  *   }else {
  *     val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)
  *
  *     unroutedNodeToInsert:Int => {
  *       val lNearestNodesOfTargetVehicle = nodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(unroutedNodeToInsert) contains x)
  *       insertPointUnroutedFirst(
  *         () => List(unroutedNodeToInsert),
  *         () => _ => lNearestNodesOfTargetVehicle,
  *         myVRP,
  *         hotRestart = false,
  *         selectInsertionPointBehavior = Best(),
  *         positionIndependentMoves = true //compulsory because we are in VLSN
  *       )
  *     }
  *   }
  * }
  *
  * def movePointVLSN(targetVehicle: Int):(Int => Neighborhood) = {
  *   if(vehicletoWorkload(targetVehicle).value + serviceTimePerNode > maxWorkloadPerVehicle){
  *     (_ => NoMoveNeighborhood)
  *   }else {
  *     val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)
  *
  *     nodeToMove:Int => {
  *       val lNearestNodesOfTargetVehicle = nodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(nodeToMove) contains x)
  *       onePointMove(
  *         () => List(nodeToMove),
  *         () => _ => lNearestNodesOfTargetVehicle,
  *         myVRP,
  *         selectDestinationBehavior = Best(),
  *         hotRestart = false,
  *         positionIndependentMoves = true  //compulsory because we are in VLSN
  *       )
  *     }
  *   }
  * }
  *
  * def removePointVLSN(node: Int) =
  *   removePoint(
  *     () => List(node),
  *     myVRP,
  *     positionIndependentMoves = true,
  *     hotRestart = false)
  *
  * def threeOptOnVehicle(vehicle:Int) = {
  *   val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(vehicle)
  *   //insertions points are position where we perform the insert,
  *   // basically the segment will start in plae of the insertion point and the insertion point will be moved upward
  *   val nodesOfTargetVehicleButVehicle = nodesOfTargetVehicle.filter(_ != vehicle)
  *   val insertionPoints = if(vehicle != v-1) vehicle+1 :: nodesOfTargetVehicleButVehicle else nodesOfTargetVehicleButVehicle
  *
  *   threeOpt(() => insertionPoints,
  *     () => _ => nodesOfTargetVehicleButVehicle,
  *     myVRP,
  *     breakSymmetry = false)
  * }
  *
  * def removeAndReInsertVLSN(pointToRemove: Int): (() => Unit) = {
  *   val checkpointBeforeRemove = myVRP.routes.defineCurrentValueAsCheckpoint(true)
  *   require(pointToRemove >= v, "cannot remove vehicle point: " + v)
  *
  *   myVRP.routes.value.positionOfAnyOccurrence(pointToRemove) match {
  *     case None => throw new Error("cannot remove non routed point:" + pointToRemove)
  *     case Some(positionOfPointToRemove) =>
  *       myVRP.routes.remove(positionOfPointToRemove)
  *   }
  *
  *   def restoreAndRelease: (() => Unit) = () => {
  *     myVRP.routes.rollbackToTopCheckpoint(checkpointBeforeRemove)
  *     myVRP.routes.releaseTopCheckpoint()
  *   }
  *
  *   restoreAndRelease
  * }
  *
  * new VLSN(
  *   v,
  *   () => {
  *     SortedMap.empty[Int, SortedSet[Int]] ++ vehicles.map((vehicle: Int) => (vehicle, SortedSet.empty[Int] ++ myVRP.getRouteOfVehicle(vehicle).filter(_ >= v)))
  *   },
  *
  *   () => SortedSet.empty[Int] ++ myVRP.unroutedNodes,
  *   nodeToRelevantVehicles = () => nodeToAllVehicles,
  *
  *   targetVehicleNodeToInsertNeighborhood = routeUnroutedPointVLSN,
  *   targetVehicleNodeToMoveNeighborhood = movePointVLSN,
  *   removePointVLSN,
  *   removeNodeAndReInsert = removeAndReInsertVLSN,
  *
  *   reOptimizeVehicle = Some(vehicle => Some(threeOptOnVehicle(vehicle))),
  *   useDirectInsert = true,
  *
  *   objPerVehicle,
  *   unroutedPenaltyObj,
  *   obj,
  *
  *   cycleFinderAlgoSelection = CycleFinderAlgoType.Mouthuy,
  *
  *   name="VLSN(" + l + ")"
  * )
  *}}}
  *
  * @param v the number of vehicles
  * @param initVehicleToRoutedNodesToMove a function that generates a map from vehicle to nodes that are to be moved
  *                                       (or unrouted) by this neighborhood (other nodes present on the vehicles will not be moved)
  * @param initUnroutedNodesToInsert a function generatinh the nodes that are not routed
  *                                  an that the VLSN should try and route
  * @param nodeToRelevantVehicles a map from node to the vehicles where the node can be routed.
  *                               It must be defined for each node mentioned in initVehicleToRoutedNodesToMove
  *                               and initUnroutedNodesToInsert
  * @param targetVehicleNodeToInsertNeighborhood vehicle to node to a neighborhood that try and insert the node
  *                                              on the vehicle.
  *                                              VLSN guarantees that the node is not routed
  *                                              (it was in initUnroutedNodesToInsert and not routed yet, or was unrouted)
  *                                              You'd better use best for this neighborhood, and no hot restart
  *                                              the moves returned by this neighborhood must be position-independent
  *                                              (check API of your neighborhood for that)
  * @param targetVehicleNodeToMoveNeighborhood vehicle to node to a neighborhood that try and move the node to the vehicle.
  *                                            VLSN guarantees that the node is routed, and reached by another vehicle.
  *                                             You'd better use best for this neighborhood, and no hot restart
  *                                             the moves returned by this neighborhood must be position-independent
  *                                             (check API of your neighborhood for that)
  * @param nodeToRemoveNeighborhood node to a neighborhood that try and remove the node.
  *                                  VLSN guarantees that the node is routed.
  *                                  you do not need any hot restart here
  *                                  the moves returned by this neighborhood must be position-independent (check API of your neighborhood for that)
  * @param removeNodeAndReInsert this is a particular procedure that given a node, which VLSN guarantees to be routed,
  *                              removes teh node from the route, and returns a procedure to re-insert it.
  *                              VLSN guarantees that the remove procedure will be called on a route
  *                              that is restored to its state when the node was removed.
  * @param reOptimizeVehicle an optional procedure to re-optimize the route of a vehicle that VLSN just
  *                          improved by performing some exchange with another vehicle or other atomic move of the VLSN
  *                          given a vehicle,it is expected to return an (optional again) neighborhood that is then exhausted
  *                          the returned neighborhood cannot modify the route of any other vehicle thn the one specified
  * @param useDirectInsert an optional argument. The VLSN will use a shortcut to perform simple inserts
  *                        and bypass the machinery that performs the graph analysis and (that's where time is spared)
  *                        avoid contruct part of the VLSN graph.
  *                        Set to true, wut please use a name for this arameter because it is going to disappear in future versions.
  * @param vehicleToObjective an array of size v that gives the objective function per vehicle. it must incorporate the strong constraint as well.
  * @param unroutedPenalty the penalty for unrouted nodes
  * @param globalObjective the global objective, which must be a sum of the above objective functions
  *                        (you can of course re-factor so that he strong constraints appear only once)
  * @param cycleFinderAlgoSelection the cycle finder algo to use. Mouthy is the fastest. In some rara case, you might experiment with MouthuyAndThenDFS.
  *                                 DFS is complete, but slower than Mouthuy
  * @param name a name toat will be used in pretty printing.
  */
class VLSN(v:Int,
           initVehicleToRoutedNodesToMove:() => SortedMap[Int,SortedSet[Int]],
           initUnroutedNodesToInsert:() => SortedSet[Int],
           nodeToRelevantVehicles:() => Map[Int,Iterable[Int]],

           // puisqu'on fait pleuiseurs inserts de nodes différents sur le même véhicule.
           targetVehicleNodeToInsertNeighborhood:Int => Int => Neighborhood,
           targetVehicleNodeToMoveNeighborhood:Int => Int => Neighborhood,
           nodeToRemoveNeighborhood:Int => Neighborhood,

           removeNodeAndReInsert:Int => () => Unit,

           reOptimizeVehicle:Option[Int => Option[Neighborhood]],
           useDirectInsert:Boolean,

           vehicleToObjective:Array[Objective],
           unroutedPenalty:Objective,
           globalObjective:Objective,
           cycleFinderAlgoSelection:CycleFinderAlgoType = CycleFinderAlgoType.Mouthuy,
           name:String = "VLSN") extends Neighborhood {

  override def getMove(obj: Objective,
                       initialObj: Int,
                       acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {

    val initialSolution = obj.model.solution(true)

    var somethingDone: Boolean = false

    var dataForRestartOpt =  doVLSNSearch(
      initVehicleToRoutedNodesToMove(),
      initUnroutedNodesToInsert(),
      None)

    //we restart with incremental restart as much as posible
    while(dataForRestartOpt match {
      case None => false
      case Some(dataForRestart) =>
        somethingDone = true
        dataForRestartOpt = restartVLSNIncrementally(oldGraph = dataForRestart.oldGraph,
          performedMoves = dataForRestart.performedMoves,
          oldVehicleToRoutedNodesToMove = dataForRestart.oldVehicleToRoutedNodesToMove,
          oldUnroutedNodesToInsert = dataForRestart.oldUnroutedNodesToInsert)
        true
    })()


    if (somethingDone) {
      val finalSolution = obj.model.solution(true)
      val finalObj = obj.value

      initialSolution.restoreDecisionVariables()

      MoveFound(LoadSolutionMove(finalSolution, finalObj, name))
    } else {
      NoMoveFound
    }
  }


  private def doVLSNSearch(vehicleToRoutedNodesToMove: SortedMap[Int, SortedSet[Int]],
                           unroutedNodesToInsert: SortedSet[Int],
                           cachedExplorations: Option[CachedExplorations]): Option[DataForVLSNRestart] = {

    //first, explore the atomic moves, and build VLSN graph
    val (vlsnGraph,directEdges) = buildGraph(vehicleToRoutedNodesToMove,
      unroutedNodesToInsert,
      cachedExplorations)

    //println(vlsnGraph.statistics)

    val liveNodes = Array.fill(vlsnGraph.nbNodes)(true)

    def killNodesImpactedByCycle(cycle: List[Edge]): Unit = {
      val theImpactedVehicles = impactedVehicles(cycle)

      val impactedRoutingNodes = SortedSet.empty[Int] ++ cycle.flatMap(edge => {
        val node = edge.from.representedNode; if (node >= 0) Some(node) else None
      })

      for (vlsnNode <- vlsnGraph.nodes) {
        if ((impactedRoutingNodes contains vlsnNode.representedNode) || (theImpactedVehicles contains vlsnNode.vehicle)) {
          liveNodes(vlsnNode.nodeID) = false
        }
      }
    }

    def impactedVehicles(cycle: List[Edge]):SortedSet[Int] = SortedSet.empty[Int] ++ cycle.flatMap(edge => {
      var l:List[Int] = List.empty
      val vehicleFrom = edge.from.vehicle
      if (vehicleFrom < v && vehicleFrom >= 0) l = vehicleFrom :: Nil
      val vehicleTo = edge.to.vehicle
      if (vehicleTo < v && vehicleTo >= 0) l = vehicleTo :: Nil
      l
    })

    var acc: List[Edge] = List.empty
    var computedNewObj: Int = globalObjective.value

    def performEdgesAndKillCycles(edges:List[Edge]): Unit ={
      acc = edges ::: acc
      val delta = edges.map(edge => edge.deltaObj).sum
      require(delta < 0, "delta should be negative, got " + delta)
      computedNewObj += delta

      for(edge <- edges){
        if(edge.move != null){
          edge.move.commit()
        }
      }
      killNodesImpactedByCycle(edges)

      require(globalObjective.value == computedNewObj, "new global objective differs from computed newObj:" + globalObjective + "!=" + computedNewObj + "edges:" + edges )
    }

    //first, kill the direct edges
    for(directEdge <- directEdges){
      performEdgesAndKillCycles(List(directEdge))
    }

    while (true) {
      CycleFinderAlgo(vlsnGraph, cycleFinderAlgoSelection).findCycle(liveNodes) match {
        case Some(listOfEdge) =>
          performEdgesAndKillCycles(listOfEdge)
        case None =>
          //we did not find any move at all on the graph
          //there is no possible incremental restart for VLSN
          if (acc.isEmpty) return None
          else {
            //We have exhausted the graph, and VLSN can be restarted
            if(printTakenMoves) {
              val newMove = CompositeMove(acc.flatMap(edge => Option(edge.move)), computedNewObj, name)
              println("   - ?  " + newMove.objAfter + "   " + newMove.toString)
            }

            //println(vlsnGraph.toDOT(acc,false,true))

            //re-optimize
            reOptimizeVehicle match{
              case None => ;
              case Some(reOptimizeNeighborhoodGenerator) =>
                //re-optimizing impacted vehicles (optionnal)
                for(vehicle <- impactedVehicles(acc)){

                  val oldObjVehicle = vehicleToObjective(vehicle).value
                  val oldGlobalObjective = globalObjective.value

                  reOptimizeNeighborhoodGenerator(vehicle) match{
                    case None => ;
                    case Some(n) =>
                      n.verbose = 0
                      val nbPerformedMoves = n.doAllMoves(obj=globalObjective)
                      if((printTakenMoves && nbPerformedMoves > 0) || (printExploredNeighbors && nbPerformedMoves == 0)){
                        println(s"   - ?  " + globalObjective.value + s"   $name:ReOptimizeVehicle(vehicle:$vehicle, neighborhood:$n nbMoves:$nbPerformedMoves)")
                      }

                      val vehicleObjDelta =  vehicleToObjective(vehicle).value - oldObjVehicle
                      val globalObjDelta = globalObjective.value - oldGlobalObjective

                      require(vehicleObjDelta == globalObjDelta,
                        "re-optimization of vehicle " + vehicle + " wih" + n + " did impact other vehicle")

                  }
                }
            }

            //now returns data for incremental restart of VLSN
            return Some(DataForVLSNRestart(
              vlsnGraph,
              acc,
              vehicleToRoutedNodesToMove: SortedMap[Int, SortedSet[Int]],
              unroutedNodesToInsert: SortedSet[Int]))
          }
      }
    }
    throw new Error("should not reach this")
  }


  case class DataForVLSNRestart(oldGraph: VLSNGraph,
                                performedMoves: List[Edge],
                                oldVehicleToRoutedNodesToMove: SortedMap[Int, SortedSet[Int]],
                                oldUnroutedNodesToInsert: SortedSet[Int])

  private def restartVLSNIncrementally(oldGraph: VLSNGraph,
                                       performedMoves: List[Edge],
                                       oldVehicleToRoutedNodesToMove: SortedMap[Int, SortedSet[Int]],
                                       oldUnroutedNodesToInsert: SortedSet[Int]):Option[DataForVLSNRestart] = {

    val (updatedVehicleToRoutedNodesToMove, updatedUnroutedNodesToInsert) =
      updateZones(performedMoves: List[Edge],
        oldVehicleToRoutedNodesToMove: SortedMap[Int, SortedSet[Int]],
        oldUnroutedNodesToInsert: SortedSet[Int])

    val cachedExplorations: Option[CachedExplorations] =
      CachedExplorations(
        oldGraph,
        performedMoves,
        v)

    doVLSNSearch(updatedVehicleToRoutedNodesToMove,
      updatedUnroutedNodesToInsert,
      cachedExplorations)
  }

  private def updateZones(performedMoves: List[Edge],
                          vehicleToRoutedNodesToMove: SortedMap[Int, SortedSet[Int]],
                          unroutedNodesToInsert: SortedSet[Int]): (SortedMap[Int, SortedSet[Int]], SortedSet[Int]) = {

    performedMoves match {
      case Nil => (vehicleToRoutedNodesToMove, unroutedNodesToInsert)
      case edge :: tail =>

        val fromNode = edge.from
        val toNode = edge.to

        edge.moveType match {
          case InsertNoEject =>
            val targetVehicle = toNode.vehicle
            val insertedNode = fromNode.representedNode

            updateZones(
              tail,
              vehicleToRoutedNodesToMove + (targetVehicle -> (vehicleToRoutedNodesToMove.getOrElse(targetVehicle, SortedSet.empty) + insertedNode)),
              unroutedNodesToInsert - insertedNode
            )

          case InsertWithEject =>
            val targetVehicle = toNode.vehicle
            val insertedNode = fromNode.representedNode
            val ejectedNode = toNode.representedNode

            updateZones(
              tail,
              vehicleToRoutedNodesToMove + (targetVehicle -> (vehicleToRoutedNodesToMove.getOrElse(targetVehicle, SortedSet.empty) + insertedNode - ejectedNode)),
              unroutedNodesToInsert - insertedNode
            )

          case MoveNoEject =>
            val fromVehicle = fromNode.vehicle
            val targetVehicle = toNode.vehicle
            val movedNode = fromNode.representedNode

            updateZones(
              tail,
              vehicleToRoutedNodesToMove
                + (targetVehicle -> (vehicleToRoutedNodesToMove.getOrElse(targetVehicle, SortedSet.empty) + movedNode))
                + (fromVehicle -> (vehicleToRoutedNodesToMove(fromVehicle) - movedNode)),
              unroutedNodesToInsert
            )
          case MoveWithEject =>
            val fromVehicle = fromNode.vehicle
            val targetVehicle = toNode.vehicle
            val movedNode = fromNode.representedNode
            val ejectedNode = toNode.representedNode

            updateZones(
              tail,
              vehicleToRoutedNodesToMove
                + (targetVehicle -> (vehicleToRoutedNodesToMove.getOrElse(targetVehicle, SortedSet.empty) + movedNode - ejectedNode))
                + (fromVehicle -> (vehicleToRoutedNodesToMove(fromVehicle) - movedNode)),
              unroutedNodesToInsert
            )

          case Remove =>
            val fromVehicle = fromNode.vehicle
            val removedNode = fromNode.representedNode

            updateZones(
              tail,
              vehicleToRoutedNodesToMove
                + (fromVehicle -> (vehicleToRoutedNodesToMove(fromVehicle) - removedNode)),
              unroutedNodesToInsert + removedNode
            )

          case SymbolicTrashToInsert | SymbolicVehicleToTrash | SymbolicTrashToNodeForEject =>
            //nothing to do here
            updateZones(tail, vehicleToRoutedNodesToMove, unroutedNodesToInsert)

        }
    }
  }

  private def buildGraph(vehicleToRoutedNodesToMove: SortedMap[Int, SortedSet[Int]],
                         unroutedNodesToInsert: SortedSet[Int],
                         cachedExplorations: Option[CachedExplorations]): (VLSNGraph,List[Edge]) = {

    cachedExplorations match {
      case None =>
        new MoveExplorerAlgo(
          v: Int,
          vehicleToRoutedNodesToMove,
          unroutedNodesToInsert,
          nodeToRelevantVehicles(),

          targetVehicleNodeToInsertNeighborhood,
          targetVehicleNodeToMoveNeighborhood,
          nodeToRemoveNeighborhood,
          removeNodeAndReInsert,
          useDirectInsert,

          vehicleToObjective,
          unroutedPenalty,
          globalObjective).buildGraph()
      case Some(cache) =>
        new IncrementalMoveExplorerAlgo(
          v: Int,
          vehicleToRoutedNodesToMove,
          unroutedNodesToInsert,
          nodeToRelevantVehicles(),

          targetVehicleNodeToInsertNeighborhood,
          targetVehicleNodeToMoveNeighborhood,
          nodeToRemoveNeighborhood,
          removeNodeAndReInsert,
          useDirectInsert,

          vehicleToObjective,
          unroutedPenalty,
          globalObjective,
          cache).buildGraph()
    }
  }
}