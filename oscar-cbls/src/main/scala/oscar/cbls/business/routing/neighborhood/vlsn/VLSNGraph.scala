package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.core.search.{DoNothingMove, Move}

class VLSNGraphBuilder(nodes:Array[Node]){
  val nbNodes = nodes.length
  val edges:Array[Array[Edge]] = Array.tabulate(nbNodes)(_ => Array.fill(nbNodes)(null))
  var fromToWithEdge:List[(Int,Int)] = List.empty
  var nextEdgeID:Int = 0
  def addEdge(from:Node,to:Node,gain:Int,move:Move = new DoNothingMove(Int.MaxValue)){
    val existingEdge = edges(from.nodeID)(to.nodeID)
    if(existingEdge == null){
      edges(from.nodeID)(to.nodeID) = new Edge(from:Node,to:Node, move:Move,gain:Int, nextEdgeID)
      nextEdgeID += 1
      fromToWithEdge = (from.nodeID,to.nodeID) :: fromToWithEdge
    }else if (existingEdge.deltaObj < gain){
      //override
      System.err.println("overriding edge in VLSN?")
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


class VLSNGraph(val nodes:Array[Node],val edges:Array[Edge]){
  val nbNodes = nodes.length
  val nbEdges = edges.length

  override def toString: String = "VLSNGraph(nbNodes:" + nbNodes + ",nbEdges:" + nbEdges +
  "\n\tnodes{\n\t\t"+ nodes.mkString("\n\t\t") + "\n\t}" +
  "\n\tedges{\n\t\t" + edges.mkString("\n\t\t") + "\n\t}" + "\n}"
}

class Node(val nodeID:Int, val representedNode:Int){
  var incoming:List[Edge] = List.empty
  var outgoing:List[Edge] = List.empty

  override def toString: String = "Node(nodeID:" + nodeID + ",representedNode:" + representedNode + ")"
}
class Edge(val from:Node, val to:Node, val move:Move, val deltaObj:Int, val edgeID:Int){

  def registerToNodes(): Unit ={
    from.outgoing = this :: from.outgoing
    to.incoming = this :: to.incoming
  }

  override def toString: String = "Edge(from:" + from.nodeID + ",to:"+to.nodeID + ",delta:" + deltaObj + ")"
}


object VLSNGraphTest extends App{

  def buildGraph():VLSNGraph = {
    val nbNodes = 6
    val nodes = Array.tabulate(nbNodes)(nodeID => new Node(nodeID, nodeID))
    val builder = new VLSNGraphBuilder(nodes: Array[Node])

    def edge(from: Int, to: Int, gain: Int): Unit = {
      builder.addEdge(nodes(from), nodes(to), gain)
    }

    edge(0, 1, -1)
    edge(1, 2, -1)
    edge(2, 3, -1)
    edge(3, 4, -1)
    edge(4, 0, -1)
    edge(0, 3, 1)
    edge(2, 4, 1)
    edge(2, 5, 1)
    edge(5, 0, 1)

    builder.finish()
  }
  println(buildGraph())
}