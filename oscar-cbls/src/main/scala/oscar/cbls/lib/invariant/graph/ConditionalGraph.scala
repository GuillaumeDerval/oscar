package oscar.cbls.lib.invariant.graph

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
case class ConditionalGraph(edges:Array[Edge],
                            nodes:Array[Node],
                            nbConditions:Int){
  val nbNodes = nodes.length
  val nbEdges = edges.length
  val conditionToConditionalEdges:Array[Edge] = Array.fill[Edge](nbConditions)(null)
  for(edge <- edges){
    edge.conditionID match{
      case None => ;
      case Some(c) => conditionToConditionalEdges(c) = edge
    }
  }
}

case class Edge(edgeId:Int,
                nodeA:Node,
                nodeB:Node,
                length:Int,
                conditionID:Option[Int]){
  require(length > 0)
  require(nodeA != nodeB)

  nodeA.registerEdge(this)
  nodeB.registerEdge(this)

  def otherNode(node:Node):Node = if(node == nodeA) nodeB else nodeA
}

case class Node(nodeId:Int){
  var incidentEdges:QList[Edge] = null
  def registerEdge(edge:Edge) {incidentEdges = QList(edge,incidentEdges)}
}


