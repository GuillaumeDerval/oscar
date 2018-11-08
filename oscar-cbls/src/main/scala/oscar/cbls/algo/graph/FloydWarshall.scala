package oscar.cbls.algo.graph

object FloydWarshall{

  def buildDistanceMatrix(g:ConditionalGraph,
                           isConditionalEdgeOpen:Int => Boolean):Array[Array[Int]] = {
    val m = buildAdjacencyMatrix(g:ConditionalGraph,
      isConditionalEdgeOpen:Int => Boolean)
    saturateAdjacencyMatrixToDistanceMatrix(m)
    m
  }

  def buildAdjacencyMatrix(g:ConditionalGraph,
                           isConditionalEdgeOpen:Int => Boolean):Array[Array[Int]] = {

    def isEdgeOpen(edge: Edge): Boolean =
      edge.conditionID match {
        case None => true
        case Some(condition) => isConditionalEdgeOpen(condition)
      }

    val n = g.nbNodes
    val matrix:Array[Array[Int]] = Array.tabulate(n)(_ => Array.fill(n)(Int.MaxValue))

    for(node <- g.nodes.indices){
      matrix(node)(node) = 0
    }

    for(edge <- g.edges if isEdgeOpen(edge)){
      val sl = edge.length min matrix(edge.nodeA.nodeId)(edge.nodeB.nodeId)
      matrix(edge.nodeA.nodeId)(edge.nodeB.nodeId) = sl
      matrix(edge.nodeB.nodeId)(edge.nodeA.nodeId) = sl
    }

    matrix
  }

  def saturateAdjacencyMatrixToDistanceMatrix(w:Array[Array[Int]]){
    val n = w.length

    for (k <- 0 to n-1) {
      for (i <- 0 to n-1) {
        for (j <- i to n-1) {

          if(w(i)(k) != Int.MaxValue && w(k)(j)!= Int.MaxValue) {
            val newDistance = w(i)(k) + w(k)(j)
            if (newDistance < w(i)(j)) {
              w(i)(j) = newDistance
              w(j)(i) = newDistance
            }
          }
        }
      }
    }
  }
}
