package oscar.cbls.algo.graph

import oscar.cbls.algo.quick.QList

/**
  *
  * @param edges edges. Some of them are conditional, a condition can only appear in one edge,
  *              and all conditions in the declared range must appear in one edge.
  * @param nodes
  * @param nbConditions the number of conditions.
  *                     Conditions are labeled from 0 to nbCondition-1 inclusive.
  *                     all conditions mut appear in one edge.
  */
case class ConditionalGraph(nodes:Array[Node],
                            edges:Array[Edge],
                            nbConditions:Int){
  val nbNodes = nodes.length
  val nbEdges = edges.length
  val nodeRange = 0 until nbNodes

  val conditionToConditionalEdges:Array[Edge] = Array.fill[Edge](nbConditions)(null)
  for(edge <- edges){
    edge.conditionID match{
      case None => ;
      case Some(c) =>
        require(conditionToConditionalEdges(c) == null)
        conditionToConditionalEdges(c) = edge
    }
  }
  require(conditionToConditionalEdges.forall(_ != null))
}

class ConditionalGraphWithIntegerNodeCoordinates(nodes:Array[Node],
                                                 edges:Array[Edge],
                                                 nbConditions:Int,
                                                 val coordinates:Array[(Int,Int)])
  extends ConditionalGraph(nodes:Array[Node],
    edges:Array[Edge],
    nbConditions:Int){
  require(nodes.length == coordinates.length)
}

class Edge(val edgeId:Int,
           val nodeA:Node,
           val nodeB:Node,
           val length:Long,
           val conditionID:Option[Int]){
  require(length > 0)
  require(nodeA != nodeB)

  nodeA.registerEdge(this)
  nodeB.registerEdge(this)

  def otherNode(node:Node):Node = if(node == nodeA) nodeB else nodeA
}

class Node(val nodeId:Int, val transitAllowed:Boolean = true){
  var incidentEdges:QList[Edge] = null
  def registerEdge(edge:Edge) {incidentEdges = QList(edge,incidentEdges)}

  override def toString: String = "Node(nodeId:" + nodeId + ")"
}


