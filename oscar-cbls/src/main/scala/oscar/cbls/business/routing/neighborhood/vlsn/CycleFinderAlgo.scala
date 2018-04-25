package oscar.cbls.business.routing.neighborhood.vlsn

class CycleFinderAlgoFloyd(graph:VLSNGraph){
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

  private def runFloydWarshallReturnNegCycleIfAny(adjacencyMatrix : Array[Array[Int]],selectedEdges:Array[Array[Edge]]):Option[List[Edge]] = {
    for (k <- nodeRange) {
      for (i <- nodeRange) {
        for (j <- nodeRange) {
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

object CycleFinderAlgoTest extends App{
  val graph = VLSNGraphTest.buildGraph()
  println(graph)

  val cycle = new CycleFinderAlgoFloyd(graph).findCycle()

  println(cycle)

}
