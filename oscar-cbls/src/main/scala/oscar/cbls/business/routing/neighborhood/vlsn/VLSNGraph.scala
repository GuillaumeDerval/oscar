package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.core.search.Move

class VLSNGraphBuilder(nodes:Array[Node]){
  val nbNodes = nodes.length
  val edges:Array[Array[Edge]] = Array.tabulate(nbNodes)(_ => Array.fill(nbNodes)(null))
  var fromToWithEdge:List[(Int,Int)] = List.empty
  var nextEdgeID:Int = 0
  def addEdge(from:Node,to:Node,move:Move,gain:Int){
    val existingEdge = edges(from.nodeID)(to.nodeID)
    if(existingEdge == null){
      edges(from.nodeID)(to.nodeID) = new Edge(from:Node,to:Node, move:Move,gain:Int, nextEdgeID)
      nextEdgeID += 1
      fromToWithEdge = (from.nodeID,to.nodeID) :: fromToWithEdge
    }else if (existingEdge.deltaObj < gain){
      //override
      System.err.println("overriding edge in VNLN??")
      edges(from.nodeID)(to.nodeID) = new Edge(from:Node,to:Node, move:Move,gain:Int, existingEdge.edgeID)
    }
  }
  def finish():VLSNGraph = {
    val edgeArray:Array[Edge] = Array.fill(nextEdgeID)(null)

    for((from,to) <- fromToWithEdge){
      val edge = edges(from)(to)
      edge.registerToNodes()
      edgeArray(edge.edgeID) = edge
    }

    new VLSNGraph(nodes,edgeArray)
  }
}


class VLSNGraph(val nodes:Array[Node],val edges:Array[Edge])

class Node(val nodeID:Int, val representedNode:Int){
  var incoming:List[Edge] = List.empty
  var outgoing:List[Edge] = List.empty
}
class Edge(val from:Node, val to:Node, val move:Move, val deltaObj:Int, val edgeID:Int){

  def registerToNodes(): Unit ={
    from.outgoing = this :: from.outgoing
    to.incoming = this :: to.incoming
  }
}
