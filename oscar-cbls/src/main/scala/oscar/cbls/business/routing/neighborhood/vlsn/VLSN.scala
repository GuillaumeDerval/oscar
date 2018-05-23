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
class VLSN(v:Int,
           initVehicleToRoutedNodesToMove:() => SortedMap[Int,SortedSet[Int]],
           initUnroutedNodesToInsert:() => SortedSet[Int],
           nodeToRelevantVehicles:() => Map[Int,Iterable[Int]],

           //TODO refactoriser çà en vehicle => noe => neighborhood, comme çà on peut mutaliser des calculs faits sur un véhicule,
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
          if (acc.isEmpty) return None
          else {
            //We have exhausted the graph, and VLSN can be restarted
            if(printTakenMoves) {
              val newMove = CompositeMove(acc.flatMap(edge => Option(edge.move)), computedNewObj, name)
              println("   - ?  " + newMove.objAfter + "   " + newMove.toString)
            }

            //println(vlsnGraph.toDOT(acc,false,true))
            //do not perform it, it is already performed!
            //then return and tell that we can restart

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

            //now starts the incremental VLSN
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

          case SymbolicTrashToInsert | SymbolicVehicleToTrash | SymbolicTrashToNodeForEject => ;
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