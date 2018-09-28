package oscar.cbls.lib.invariant.graph

import oscar.cbls.algo.quick.QList

case class ConditionalGraph(edges:Array[Edge],
                            nodes:Array[Node]){
  val nbNodes = nodes.length
  val nbEdges = edges.length
}

case class Edge(edgeId:Int,
                nodeA:Node,
                nodeB:Node,
                length:Int,
                conditionID:Option[Int]){

  nodeA.registerEdge(this)
  nodeB.registerEdge(this)
  def otherNode(node:Node):Node = if(node == nodeA) nodeB else nodeA
}

case class Node(nodeId:Int){
  var incidentEdges:QList[Edge] = null
  def registerEdge(edge:Edge) {incidentEdges = QList(edge,incidentEdges)}
}


