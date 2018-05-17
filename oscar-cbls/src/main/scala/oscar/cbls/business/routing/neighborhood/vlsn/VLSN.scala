package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.Objective
import oscar.cbls.business.routing.neighborhood.vlsn.CycleFinderAlgoType.CycleFinderAlgoType
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
           cycleFinderAlgoSelection:CycleFinderAlgoType = CycleFinderAlgoType.Mouthuy,
           exhaustVLSN:Boolean =  true,
           name:String = "VLSN",
          ) extends Neighborhood {



  override def getMove(obj: Objective,
                       initialObj: Int,
                       acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {

    require(initialObj == globalObjective.value)

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

    //println(vlsnGraph.statistics)

    val liveNodes = Array.fill(vlsnGraph.nbNodes)(true)

    def killNodesImpactedByCycle(cycle:List[Edge]): Unit ={
      val impactedVehicles = SortedSet.empty[Int] ++ cycle.flatMap(edge => {val vehicle = edge.from.vehicle; if (vehicle < v && vehicle >=0) Some(vehicle) else None})
      val impactedRoutingNodes = SortedSet.empty[Int] ++ cycle.flatMap(edge => {val node = edge.from.representedNode ; if (node >= 0) Some(node) else None})

      for(vlsnNode <- vlsnGraph.nodes){
        if((impactedRoutingNodes contains vlsnNode.representedNode) || (impactedVehicles contains vlsnNode.vehicle)){
          liveNodes(vlsnNode.nodeID) = false
        }
      }
    }

    if(exhaustVLSN){
      var acc:List[Edge] = List.empty
      var computedNewObj:Int = initialObj
      while(true){
        CycleFinderAlgo(vlsnGraph, cycleFinderAlgoSelection).findCycle(liveNodes) match {
          case None =>
            if(acc.isEmpty) return NoMoveFound
            else{
              // println(vlsnGraph.toDOT(acc,false,true))
              return MoveFound(CompositeMove(acc.flatMap(edge => Option(edge.move)), computedNewObj, name))
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
    }else {
      //then, find proper negative cycle in graph
      CycleFinderAlgo(vlsnGraph, cycleFinderAlgoSelection).findCycle(liveNodes) match {
        case None =>
          NoMoveFound
        case Some(listOfEdge) =>
          require(listOfEdge.nonEmpty, "list of edge should not be empty")
          //TODO:further explore the graph, to find all independent neg cycles, and improve added value.
          //finally, extract the moves from the graph and return the composite moves

          val moves = listOfEdge.flatMap(edge => Option(edge.move))
          val delta = listOfEdge.map(edge => edge.deltaObj).sum
          require(delta < 0, "delta should be negative, got " + delta)
          val computedNewObj = initialObj + delta
          require(computedNewObj < initialObj, "no gain in VLSN?")
          MoveFound(CompositeMove(moves, computedNewObj, name))
      }
    }
  }
}

