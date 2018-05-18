package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.Objective
import oscar.cbls.business.routing.neighborhood.vlsn.CycleFinderAlgoType.CycleFinderAlgoType
import oscar.cbls.core.search._

import scala.collection.immutable.{SortedMap, SortedSet}
import oscar.cbls.business.routing.neighborhood.vlsn.VLSNMoveType._


/*
all neighborhood must return moves that are position-independent.
by default this is not the case. A trait has been added here to ensure that moves are indeed position-independent
 */
class IncrementalVLSN(v:Int,
                      initVehicleToRoutedNodesToMove:() => SortedMap[Int,SortedSet[Int]],
                      initUnroutedNodesToInsert:() => SortedSet[Int],
                      nodeToRelevantVehicles:() => Map[Int,Iterable[Int]],

                      nodeVehicleToInsertNeighborhood:(Int,Int) => Neighborhood,
                      nodeTargetVehicleToMoveNeighborhood:(Int,Int) => Neighborhood,
                      nodeToRemoveNeighborhood:Int => Neighborhood,
                      removeNodeAndReInsert:Int => () => Unit,

                      reOptimizeVehicle:Int => Option[Neighborhood],

                      vehicleToObjective:Array[Objective],
                      unroutedPenalty:Objective,
                      globalObjective:Objective,
                      cycleFinderAlgoSelection:CycleFinderAlgoType = CycleFinderAlgoType.Mouthuy,
                      name:String = "IncrementalVLSN",
                     ) extends Neighborhood {


  override def getMove(obj: Objective,
                       initialObj: Int,
                       acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {

    val initialSolution = obj.model.solution(true)

    val somethingDone: Boolean = doVLSNSearch(
      initVehicleToRoutedNodesToMove(),
      initUnroutedNodesToInsert(),
      None)

    if (somethingDone) {
      val finalSolution = obj.model.solution(true)
      val finalObj = obj.value

      initialSolution.restoreDecisionVariables()

      MoveFound(LoadSolutionMove(finalSolution, finalObj, name))
    } else {
      NoMoveFound
    }
  }


  def doVLSNSearch(vehicleToRoutedNodesToMove: SortedMap[Int, SortedSet[Int]],
                   unroutedNodesToInsert: SortedSet[Int],
                   cachedExplorations: Option[CachedExplorations]): Boolean = {

    //first, explore the atomic moves, and build VLSN graph
    val vlsnGraph = buildGraph(vehicleToRoutedNodesToMove,
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
      val vehicle = edge.from.vehicle; if (vehicle < v && vehicle >= 0) Some(vehicle) else None
    })

    var acc: List[Edge] = List.empty
    var computedNewObj: Int = globalObjective.value

    while (true) {
      CycleFinderAlgo(vlsnGraph, cycleFinderAlgoSelection).findCycle(liveNodes) match {
        case None =>
          if (acc.isEmpty) return false
          else {

            //compose new move and commit it
            val newMove = CompositeMove(acc.flatMap(edge => Option(edge.move)), computedNewObj, name)
            newMove.commit()
            if(printTakenMoves) {
              println("   - ?  " + newMove.objAfter + "   " + newMove.toString)
            }

            //re-optimizing impacted vehicles (optionnal)
            for(vehicle <- impactedVehicles(acc)){
              reOptimizeVehicle(vehicle) match{
                case None => ;
                case Some(n) =>
                  n.verbose = 0
                  val nbPerformedMoves = n.doAllMoves(obj=globalObjective)
                  if((printTakenMoves && nbPerformedMoves > 0) || (printExploredNeighbors && nbPerformedMoves == 0)){
                    println(s"   - ?  " + globalObjective.value + s"   $name:ReOptimizeVehicle(vehicle:$vehicle, neighborhood:$n nbMoves:$nbPerformedMoves)")
                  }
              }
            }

            //now starts the incremental VLSN
            restartVLSNIncrementally(
              vlsnGraph,
              acc,
              vehicleToRoutedNodesToMove: SortedMap[Int, SortedSet[Int]],
              unroutedNodesToInsert: SortedSet[Int])

            return true
          }
        case Some(listOfEdge) =>
          val delta = listOfEdge.map(edge => edge.deltaObj).sum
          require(delta < 0, "delta should be negative, got " + delta)
          computedNewObj += delta
          acc = acc ::: listOfEdge
          killNodesImpactedByCycle(listOfEdge)
      }
    }
    throw new Error("should not reach this")
  }


  def restartVLSNIncrementally(oldGraph: VLSNGraph,
                               performedMoves: List[Edge],
                               oldVehicleToRoutedNodesToMove: SortedMap[Int, SortedSet[Int]],
                               oldUnroutedNodesToInsert: SortedSet[Int]) {

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


  def updateZones(performedMoves: List[Edge],
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

          case Symbolic => ;
            updateZones(tail, vehicleToRoutedNodesToMove, unroutedNodesToInsert)

        }
    }
  }

  def buildGraph(vehicleToRoutedNodesToMove: SortedMap[Int, SortedSet[Int]],
                 unroutedNodesToInsert: SortedSet[Int],
                 cachedExplorations: Option[CachedExplorations]): VLSNGraph = {

    cachedExplorations match {
      case None =>
        new MoveExplorerAlgo(
          v: Int,
          vehicleToRoutedNodesToMove,
          unroutedNodesToInsert,
          nodeToRelevantVehicles(),

          nodeVehicleToInsertNeighborhood,
          nodeTargetVehicleToMoveNeighborhood,
          nodeToRemoveNeighborhood,
          removeNodeAndReInsert,

          vehicleToObjective,
          unroutedPenalty,
          globalObjective).buildGraph()
      case Some(cache) =>
        new IncrementalMoveExplorerAlgo(
          v: Int,
          vehicleToRoutedNodesToMove,
          unroutedNodesToInsert,
          nodeToRelevantVehicles(),

          nodeVehicleToInsertNeighborhood,
          nodeTargetVehicleToMoveNeighborhood,
          nodeToRemoveNeighborhood,
          removeNodeAndReInsert,

          vehicleToObjective,
          unroutedPenalty,
          globalObjective,
          cache).buildGraph()
    }
  }
}