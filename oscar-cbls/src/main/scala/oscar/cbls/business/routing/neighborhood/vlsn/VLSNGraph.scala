package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.core.search.{DoNothingMove, Move}

import scala.collection.immutable.SortedSet

class VLSNNodeBuilder(nbLabels:Int) {

  var nodes: List[Node] = List.empty
  var nextNodeID: Int = 0

  def addNode(representedNode:Int, vehicle:Int, label:Int):Node = {
    require(label >=0 && label < nbLabels)
    val n = new Node(nextNodeID, representedNode:Int, vehicle:Int, label)
    nextNodeID += 1
    nodes = n :: nodes
    n
  }

  def finish():(Array[Node],Int) = {
    (nodes.reverse.toArray,nbLabels)
  }
}

class VLSNEdgeBuilder(nodes:Array[Node],nbLabels:Int){
  val nbNodes = nodes.length
  val edges:Array[Array[Edge]] = Array.tabulate(nbNodes)(_ => Array.fill(nbNodes)(null))
  var fromToWithEdge:List[(Int,Int)] = List.empty
  var nextEdgeID:Int = 0

  def addEdge(from:Node,to:Node,gain:Int,move:Move){
    val existingEdge = edges(from.nodeID)(to.nodeID)
    if(existingEdge == null){
      edges(from.nodeID)(to.nodeID) = new Edge(from:Node,to:Node, move:Move,gain:Int, nextEdgeID)
      nextEdgeID += 1
      fromToWithEdge = (from.nodeID,to.nodeID) :: fromToWithEdge
    }else {
      if (existingEdge.deltaObj < gain) {
        //override
        System.err.println("overriding edge in VLSN?")
        edges(from.nodeID)(to.nodeID) = new Edge(from: Node, to: Node, move: Move, gain: Int, existingEdge.edgeID)
      }
    }
  }

  def finish():VLSNGraph = {
    val edgeArray:Array[Edge] = Array.fill(nextEdgeID)(null)

    for((from,to) <- fromToWithEdge){
      val edge = edges(from)(to)
      edge.registerToNodes()
      edgeArray(edge.edgeID) = edge
    }

    new VLSNGraph(nodes,edgeArray,nbLabels)
  }
}

/**
  *
  * @param nodes
  * @param edges
  * @param nbLabels labels range from 0 to nbLabels-1
  */
class VLSNGraph(val nodes:Array[Node],val edges:Array[Edge],val nbLabels:Int){
  val nbNodes = nodes.length
  val nbEdges = edges.length

  override def toString: String = "VLSNGraph(nbNodes:" + nbNodes + ",nbEdges:" + nbEdges +
    "\n\tnodes{\n\t\t"+ nodes.mkString("\n\t\t") + "\n\t}" +
    "\n\tedges{\n\t\t" + edges.mkString("\n\t\t") + "\n\t}" + "\n}" + "\n\n\n\n" + toDOT(SortedSet(1,2,4))

  //"C:\Program Files (x86)\Graphviz2.38\bin\neato" -Tpng  vlsnGraph.dot > a.png
  def toDOT(edgesToBold:SortedSet[Int] = SortedSet.empty):String =
    "##Command to produce the output: \"neato -Tpng thisfile > thisfile.png\"\n" +
      "digraph VLSNGraph {\n" +
      nodes.map(_.toDOT(edgesToBold)).mkString("\t","\n\t","\n") +
      edges.map(edge => edge.toDOT(edgesToBold.contains(edge.edgeID))).mkString("\t","\n\t","\n") +
      "\toverlap=false\n"+
      "\tlabel=\"VLSN graph\";\n" +
      "\tfontsize=12;\n" +
      "}"
}

class Node(val nodeID:Int, val representedNode:Int, vehicle:Int, val label:Int){
  var incoming:List[Edge] = List.empty
  var outgoing:List[Edge] = List.empty

  override def toString: String = "Node(nodeID:" + nodeID + ",routingNode:" + representedNode + "vehicle:" + vehicle + ")"

  def toDOT(edgesToBold:SortedSet[Int]):String = "\"" + nodeID + "\" [shape=circle,style=filled,fillcolor=white, label = \"node:" + representedNode + "\\nvehicle:" + vehicle + "\"] ; "
}

class Edge(val from:Node, val to:Node, val move:Move, val deltaObj:Int, val edgeID:Int){
  def registerToNodes(): Unit ={
    from.outgoing = this :: from.outgoing
    to.incoming = this :: to.incoming
  }

  override def toString: String = "Edge(from:" + from.nodeID + ",to:"+to.nodeID + ",deltaObj:" + deltaObj + ")"

  def toDOT(bold:Boolean = false):String =
  "\"Edge" + edgeID + "\" [shape=rectangle,style=filled,fillcolor=gray, label=\"deltaObj:" + deltaObj + "\\n" + move + "\"" + (if(bold) " color=blue" else "") +"] ; " +
    from.nodeID + " -> " + "\"Edge" + edgeID + "\"" + (if(bold) "[color=blue]" else "") + ";" +
    "\"Edge" + edgeID + "\" -> " + to.nodeID + (if(bold) "[color=blue]" else "") + ";"
}


object VLSNGraphTest extends App{

  def buildGraph():VLSNGraph = {
    val nbNodes = 6
    val nodes = Array.tabulate(nbNodes)(nodeID => new Node(nodeID, nbNodes + nodeID,nodeID,nodeID%2))
    val builder = new VLSNEdgeBuilder(nodes: Array[Node], nbNodes) //nbLAbel is set here to nbNodes

    def edge(from: Int, to: Int, gain: Int): Unit = {
      builder.addEdge(nodes(from), nodes(to), gain,  new DoNothingMove(Int.MaxValue))
    }

    edge(0, 1, 10)
    edge(1, 2, -1)
    edge(2, 3, -1)
    edge(3, 4, 1)
    edge(4, 0, 1)
    edge(0, 3, 1)
    edge(2, 4, 1)
    edge(2, 5, -1)
    edge(5, 0, 1)
    edge(4, 2, -1)
    builder.finish()
  }
  println(buildGraph())
}