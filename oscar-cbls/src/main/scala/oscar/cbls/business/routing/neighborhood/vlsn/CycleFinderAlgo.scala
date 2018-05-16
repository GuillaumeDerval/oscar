package oscar.cbls.business.routing.neighborhood.vlsn



object MatrixTools{
  def printBoolMatrix(m:Array[Array[Boolean]]): Unit ={
    for(l <- m){
      println(l.mkString("\t"))
    }
  }
}

object CycleFinderAlgoType extends Enumeration{
  type CycleFinderAlgoType = Value
  val Mouthuy, DFS, DFSPruned, MouthuyAndThenDFS = Value
}

import CycleFinderAlgoType._

abstract class CycleFinderAlgo{
  def findCycle(liveNodes:Array[Boolean]):Option[List[Edge[Int]]]
}

object CycleFinderAlgo {

  def apply(graph: VLSNGraph[Int], algo: CycleFinderAlgoType): CycleFinderAlgo = {
    algo match {
      case Mouthuy =>
        new CycleFinderAlgoMouthuy(graph)
      case DFS =>
        new CycleFinderAlgoDFS(graph, pruneOnReachability = false)
      case DFSPruned =>
        new CycleFinderAlgoDFS(graph, pruneOnReachability = true)
      case MouthuyAndThenDFS =>
        new CycleFinderAlgoMouthuyAndThenDFS(graph:VLSNGraph[Int])
    }
  }

}

class CycleFinderAlgoMouthuyAndThenDFS(graph:VLSNGraph[Int]) extends CycleFinderAlgo{
  override def findCycle(liveNodes:Array[Boolean]): Option[List[Edge[Int]]] = {
    new CycleFinderAlgoMouthuy(graph).findCycle(liveNodes:Array[Boolean]) match{
      case None =>
        println("Mouthy stalled, reverting to DFS")
        new CycleFinderAlgoDFS(graph, pruneOnReachability = true).findCycle(liveNodes:Array[Boolean])
      case a => a
    }
  }
}