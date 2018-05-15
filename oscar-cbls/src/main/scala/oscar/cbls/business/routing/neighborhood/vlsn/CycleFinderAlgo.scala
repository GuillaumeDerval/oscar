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
  def findCycle:Option[List[Edge]]
}

object CycleFinderAlgo {

  def apply(graph: VLSNGraph, algo: CycleFinderAlgoType): CycleFinderAlgo = {
    algo match {
      case Mouthuy =>
        new CycleFinderAlgoMouthuy(graph)
      case DFS =>
        new CycleFinderAlgoDFS(graph, pruneOnReachability = false)
      case DFSPruned =>
        new CycleFinderAlgoDFS(graph, pruneOnReachability = true)
      case MouthuyAndThenDFS =>
        new CycleFinderAlgoMouthuyAndThenDFS(graph:VLSNGraph)
    }
  }

}

class CycleFinderAlgoMouthuyAndThenDFS(graph:VLSNGraph) extends CycleFinderAlgo{
  override def findCycle: Option[List[Edge]] = {
    new CycleFinderAlgoMouthuy(graph).findCycle() match{
      case None =>
        new CycleFinderAlgoDFS(graph, pruneOnReachability = true).findCycle()
      case a => a
    }
  }
}