package oscar.cbls.business.routing.neighborhood.vlsn

import scala.collection.immutable.SortedSet
import scala.collection.mutable

class CycleFinderAlgoDFS(graph:VLSNGraph,pruneOnReachability:Boolean){
  private val nodes:Array[Node] = graph.nodes
  private val nbNodes = nodes.length
  private val nbLabels = graph.nbLabels

  def findCycle():Option[List[Edge]] = {

    val reachabilityMatrix = if(pruneOnReachability){
      new ReacheabilityFloydWarshall(graph:VLSNGraph).buildRechabilityMatrix()
    }else null

    //MatrixTools.printBoolMatrix(reachabilityMatrix)

    val isNodeReached = Array.fill(nbNodes)(false)
    val isLabelReached = Array.fill(nbLabels)(false)
    var rootNode:Node = null
    var rooNodeID:Int = -1

    def dfsExplore(node:Node,summedDelta:Int):Option[List[Edge]] ={
      var outgoingEdges = node.outgoing

      while(outgoingEdges != Nil){
        val currentEdge = outgoingEdges.head
        outgoingEdges = outgoingEdges.tail

        val currentEdgeDelta = currentEdge.deltaObj
        val newSummedDelta = currentEdgeDelta + summedDelta

        val targetNode = currentEdge.to
        val targetNodeID = targetNode.nodeID

        if(targetNode == rootNode){
          if (newSummedDelta <0) {
            //we just found a negative cycle
            println("found negative cycle with summed delta: " + newSummedDelta)
            return Some(List(currentEdge))
          }else {
            //we found a cycle, but it is not negative
            //carry on the while
          }
        }else if (!pruneOnReachability || reachabilityMatrix(targetNodeID)(rooNodeID)){

          val targetLabel = targetNode.label

          if(isNodeReached(targetNodeID)) {
            //we are back on a node that we are exploring; we do nothing more
            //TODO: check for a negative cycle here
            //carry on the while

          }else {
            isNodeReached(targetNodeID) = true
            if (targetLabel == -1) {
              //this is a new node without any particular label
              //recurse
              dfsExplore(targetNode, newSummedDelta) match {
                case None => ;
                case Some(l) => return Some(currentEdge :: l)
              }
            } else if (!isLabelReached(targetLabel)) {
              //this is a new node, it has a label that has not been reached yet
              //mark and recurse
              isLabelReached(targetLabel) = true
              dfsExplore(targetNode, newSummedDelta) match {
                case None => ;
                case Some(l) => return Some(currentEdge :: l)
              }
              isLabelReached(targetLabel) = false
            }
            isNodeReached(targetNodeID) = false
          }
        }
      }
      None
    }

    for(n <- nodes){
      rootNode = n
      rooNodeID = n.nodeID
      dfsExplore(rootNode,0) match{
        case None => ;
        case a => return a
      }
    }
    None
  }
}

/*
class CycleFinderAlgoMouthy(graph:VLSNGraph){
  private val nodes:Array[Node] = graph.nodes
  private val edges:Array[Edge] = graph.edges
  private val nbNodes = nodes.length
  private val nodeRange = 0 until nbNodes


  def findCycle():Option[List[Edge]] = {
    for(n <- nodeRange){
      findRootedCycle(n) match{
        case None => ;
        case a => return a
      }
    }
    return None
  }

  def findRootedCycle(rootNode:Node):Option[List[Edge]] = {
    val distanceFromS:Array[Int] = Array.tabulate(nbNodes)(_ => Int.MaxValue)
    distanceFromS(rootNode.nodeID) = 0
    val selectedIncomingEdges:Array[Edge] = Array.fill(nbNodes)(null)

    val listOfNodesToDevelop:mutable.Queue[Node] = mutable.Queue(rootNode)

    def markPathTo(node:Node,mark:Boolean){

    }



    while(listOfNodesToDevelop.nonEmpty){
      val i = listOfNodesToDevelop.dequeue()



    }

  }

}
*/

/**
  * finds negative cycle.
  * does not care about labels
  * complete O(n^^3)
  * @param graph
  */
class CycleFinderAlgoFloydNoLabel(graph:VLSNGraph){
  private val nodes:Array[Node] = graph.nodes
  private val edges:Array[Edge] = graph.edges
  private val nbNodes = nodes.length
  private val nodeRange = 0 until nbNodes

  def findCycle():Option[List[Edge]] = {
    //we use the floyd warshall algo
    val (adjacencyMatrix,selectedEdges) = buildFloydMatrices()
    runFloydWarshallReturnNegCycleIfAny(adjacencyMatrix,selectedEdges)
  }

  def printDistanceMatrix(m:Array[Array[Int]]): Unit ={
    for(l <- m){
      println(l.mkString("\t"))
    }
  }

  private def buildFloydMatrices(): (Array[Array[Int]],Array[Array[Edge]]) = {
    val adjacencyMatrix:Array[Array[Int]] = Array.tabulate(nbNodes)(_ => Array.fill(nbNodes)(Int.MaxValue))
    val selectedEdges:Array[Array[Edge]] = Array.tabulate(nbNodes)(_ => Array.fill(nbNodes)(null))

    for(node <- nodes.indices){
      adjacencyMatrix(node)(node) = 0
    }

    for(edge <- edges){
      require(adjacencyMatrix(edge.from.nodeID)(edge.to.nodeID) == Int.MaxValue)
      adjacencyMatrix(edge.from.nodeID)(edge.to.nodeID) = edge.deltaObj
      selectedEdges(edge.from.nodeID)(edge.to.nodeID) = edge
    }

    (adjacencyMatrix,selectedEdges)
  }

  private def runFloydWarshallReturnNegCycleIfAny(adjacencyMatrix : Array[Array[Int]],
                                                  selectedEdges:Array[Array[Edge]],
                                                  liveNodes:Iterable[Int] = nodeRange):Option[List[Edge]] = {
    for (k <- liveNodes) {
      for (i <- liveNodes) {
        for (j <- liveNodes) {
          val oldValue = adjacencyMatrix (i)(j)
          val newValue =
            if (adjacencyMatrix (i)(k) == Int.MaxValue || adjacencyMatrix(k)(j) == Int.MaxValue) Int.MaxValue
            else adjacencyMatrix (i)(k) +  adjacencyMatrix(k)(j)

          if (newValue < oldValue){
            adjacencyMatrix(i)(j) = newValue
            selectedEdges(i)(j) = selectedEdges(i)(k)
          }

          if(i == j && newValue < 0) {
            //we have a negative cycle on node i (==j)
            return Some(findCycleInFloydResult(adjacencyMatrix: Array[Array[Int]], selectedEdges: Array[Array[Edge]], j))
          }
        }
      }
    }
    None
  }

  private def findCycleInFloydResult(distanceMatrix:Array[Array[Int]],selectedEdges:Array[Array[Edge]], rootNode:Int):List[Edge] = {
    var currentEdge = selectedEdges(rootNode)(rootNode)
    var toReturn:List[Edge] = List(currentEdge)
    while(currentEdge.to.nodeID != rootNode){
      currentEdge = selectedEdges(currentEdge.to.nodeID)(rootNode)
      toReturn = currentEdge :: toReturn
    }
    toReturn.reverse
  }
}


object MatrixTools{
  def printBoolMatrix(m:Array[Array[Boolean]]): Unit ={
    for(l <- m){
      println(l.mkString("\t"))
    }
  }
}

class ReacheabilityFloydWarshall(graph:VLSNGraph){
  private val nodes:Array[Node] = graph.nodes
  private val edges:Array[Edge] = graph.edges
  private val nbNodes = nodes.length
  private val nodeRange = 0 until nbNodes


  def buildRechabilityMatrix():Array[Array[Boolean]] = {
    val adjacencyMatrix = buildFloydMatrices()
    saturateAdjacencyMatrixToReacheabilityMatrix(adjacencyMatrix)
    adjacencyMatrix
  }

  private def buildFloydMatrices(): Array[Array[Boolean]] = {
    val adjacencyMatrix:Array[Array[Boolean]] = Array.tabulate(nbNodes)(_ => Array.fill(nbNodes)(false))

    for(node <- nodes.indices){
      adjacencyMatrix(node)(node) = true
    }

    for(edge <- edges){
      adjacencyMatrix(edge.from.nodeID)(edge.to.nodeID) = true
    }

    adjacencyMatrix
  }

  private def saturateAdjacencyMatrixToReacheabilityMatrix(adjacencyMatrix : Array[Array[Boolean]]):Unit = {
    for (k <- nodeRange) {
      for (i <- nodeRange) {
        for (j <- nodeRange) {
          adjacencyMatrix(i)(j) = adjacencyMatrix (i)(j) || adjacencyMatrix (i)(k) ||  adjacencyMatrix(k)(j)
        }
      }
    }
  }
}

object CycleFinderAlgoTest extends App{
  val graph = VLSNGraphTest.buildGraph()
  println(graph)

println("starting DFS")
  val cycle = new CycleFinderAlgoDFS(graph,false).findCycle()
  println("done DFS")
  println(cycle)

  println(graph.toDOT(SortedSet.empty[Int] ++ cycle.get.map(_.edgeID)))
}
