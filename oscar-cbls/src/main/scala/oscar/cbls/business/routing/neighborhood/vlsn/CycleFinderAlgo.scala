package oscar.cbls.business.routing.neighborhood.vlsn

class CycleFinderAlgo(graph:VLSNGraph){
  private val nodes:Array[Node] = graph.nodes
  private val edges:Array[Edge] = graph.edges
  private val nbNodes = nodes.length
  private val nodeRange = 0 until nbNodes

  def findCycle():List[Edge] = {
    //we use the floyd warshall algo
    val (adjacencyMatrix,selectedEdges) = buildFloydMatrices()
    runFloydWarshall(adjacencyMatrix,selectedEdges)
    findCycleInFloydResult(adjacencyMatrix,selectedEdges)
  }

  private def buildFloydMatrices(): (Array[Array[Int]],Array[Array[Edge]]) = {
    val adjacencyMatrix:Array[Array[Int]] = Array.tabulate(nbNodes)(_ => Array.fill(nbNodes)(Int.MaxValue))
    val selectedEdges:Array[Array[Edge]] = Array.tabulate(nbNodes)(_ => Array.fill(nbNodes)(null))

    for(node <- nodes.indices){
      adjacencyMatrix(node)(node) = 0
    }

    for(edge <- edges){
      val oldDistance = adjacencyMatrix(edge.from.nodeID)(edge.to.nodeID)
      val newDistance = edge.deltaObj
      if(newDistance < oldDistance) {
        adjacencyMatrix(edge.from.nodeID)(edge.to.nodeID) = newDistance
        selectedEdges(edge.from.nodeID)(edge.to.nodeID) = edge
      }
    }

    (adjacencyMatrix,selectedEdges)
  }

  private def runFloydWarshall(adjacencyMatrix : Array[Array[Int]],selectedEdges:Array[Array[Edge]]){
    for (k <- nodeRange) {
      for (i <- nodeRange) {
        for (j <- nodeRange) {
          val oldValue = adjacencyMatrix (i)(j)
          val newValue = adjacencyMatrix (i)(k) +  adjacencyMatrix(k)(j)
          if (newValue < oldValue){
            adjacencyMatrix(i)(j) = newValue
            selectedEdges(i)(j) = selectedEdges(i)(k)
          }
        }
      }
    }
  }

  private def findCycleInFloydResult(distanceMatrix:Array[Array[Int]],selectedEdges:Array[Array[Edge]]):List[Edge] = {
    val nodeswithCycle = nodeRange.filter(node => distanceMatrix(node)(node) < 0)
    if(nodeswithCycle.isEmpty) return List.empty

    //select the node with hte most negative cycle
    val rootNode = nodeswithCycle.minBy(nodeID => distanceMatrix(nodeID)(nodeID))

    var currentEdge = selectedEdges(rootNode)(rootNode)
    var toReturn:List[Edge] = List(currentEdge)
    while(currentEdge.to.nodeID != rootNode){
      currentEdge = selectedEdges(currentEdge.to.nodeID)(rootNode)
      toReturn = currentEdge :: toReturn
    }
    toReturn.reverse
  }
}
