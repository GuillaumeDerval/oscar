package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.Objective
import oscar.cbls.core.search._

import scala.collection.immutable.{SortedMap, SortedSet}


/*
all neighborhood must return moves that are position-independent.
by default this is not the case. A trait has been added here to ensure that moves are indeed position-independent
 */
class VLSN(v:Int,
           vehicleToRoutedNodesToMove:() => SortedMap[Int,Iterable[Int]],
           unroutedNodesToInsert:() => Iterable[Int],
           nodeToRelevantVehicles:() => Map[Int,Iterable[Int]],

           nodeVehicleToInsertNeighborhood:(Int,Int) => Neighborhood,
           nodeTargetVehicleToMoveNeighborhood:(Int,Int) => Neighborhood,
           nodeToRemoveNeighborhood:Int => Neighborhood,
           removeNodeAndReInsert:Int => () => Unit,

           vehicleToObjective:Array[Objective],
           unroutedPenalty:Objective,
           globalObjective:Objective,
           name:String = "VLSN") extends Neighborhood {

  override def getMove(obj: Objective,
                       initialObj: Int,
                       acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {

    //first, explore the atomic moves, and build VLSN graph
    val vlsnGraph = new MoveExplorerAlgo(
      v:Int,
      vehicleToRoutedNodesToMove(),
      unroutedNodesToInsert(),
      nodeToRelevantVehicles(),

      nodeVehicleToInsertNeighborhood,
      nodeTargetVehicleToMoveNeighborhood,
      nodeToRemoveNeighborhood,
      removeNodeAndReInsert,

      vehicleToObjective,
      unroutedPenalty,
      globalObjective).buildGraph()

    println(vlsnGraph.toDOT())

    //then, find proper negative cycle in graph
    new CycleFinderAlgoMouthuy(vlsnGraph).findCycle() match{
      case None => NoMoveFound
      case Some(listOfEdge) =>
        println("found VLSN move: " + listOfEdge.map(e => e.toString + "\t" + e.move).mkString("\n\t","\n\t",""))
        //TODO:further explore the graph, to find all independent neg cycles, and improve added value.
        //finally, extract the moves from the graph and return the composite moves

        println(vlsnGraph.toDOT(listOfEdge))

        throw new Error("fini")
        val moves = listOfEdge.map(edge => edge.move)
        val delta = listOfEdge.map(edge => edge.deltaObj).sum
        MoveFound(CompositeMove(moves,initialObj + delta,name))
    }
  }
}

