package oscar.cbls.algo.graph

object FloydWarshall{

  def buildDistanceMatrix(g:ConditionalGraph,
                           isConditionalEdgeOpen:Int => Boolean):Array[Array[Option[Int]]] = {
    val m = buildAdjacencyMatrix(g:ConditionalGraph,
      isConditionalEdgeOpen:Int => Boolean)
    saturateAdjacencyMatrixToDistanceMatrix(m)
    m
  }

  def buildAdjacencyMatrix(g:ConditionalGraph,
                           isConditionalEdgeOpen:Int => Boolean):Array[Array[Option[Int]]] = {

    def isEdgeOpen(edge: Edge): Boolean =
      edge.conditionID match {
        case None => true
        case Some(condition) => isConditionalEdgeOpen(condition)
      }

    val n = g.nbNodes
    val matrix:Array[Array[Option[Int]]] = Array.tabulate(n)(_ => Array.fill(n)(None))

    for(node <- g.nodes.indices){
      matrix(node)(node) = Some(0)
    }

    for(edge <- g.edges if isEdgeOpen(edge)){

      val sl = matrix(edge.nodeA.nodeId)(edge.nodeB.nodeId) match{
        case None => Some(edge.length)
        case Some(d) => Some(edge.length min d)
      }

      matrix(edge.nodeA.nodeId)(edge.nodeB.nodeId) = sl
      matrix(edge.nodeA.nodeId)(edge.nodeB.nodeId) = sl
    }

    matrix
  }

  def saturateAdjacencyMatrixToDistanceMatrix(w:Array[Array[Option[Int]]]){
    val n = w.length

    for (k <- 0 to n-1) {
      for (i <- 0 to n-1) {
        for (j <- i+1 to n-1) {

          if(w(i)(k).isDefined && w(k)(j).isDefined) {
            val newDistance = w(i)(k).get + w(k)(j).get
            if (w(i)(j).isEmpty || newDistance <= w(i)(j).get) {
              w(i)(j) = Some(newDistance)
              w(j)(i) = w(i)(j)
            }
          }
        }
      }
    }
  }
}
