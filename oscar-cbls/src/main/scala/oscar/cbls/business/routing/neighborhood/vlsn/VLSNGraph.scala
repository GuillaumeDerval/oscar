package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.core.search.{DoNothingMove, Move}

import scala.collection.immutable.SortedSet

object VLSNMoveType extends Enumeration{
  type VLSNMoveType = Value
  val InsertNoEject, InsertWithEject, MoveNoEject, MoveWithEject, Remove, SymbolicTrashToInsert,SymbolicVehicleToTrash,SymbolicTrashToNodeForEject = Value
}

import VLSNMoveType._

object VLSNSNodeType extends Enumeration {
  type VLSNSNodeType = Value
  val RegularNode, VehicleNode, UnroutedNode, FictiveNode = Value
}

import VLSNSNodeType._


class VLSNNodeBuilder(var nbLabels:Int) {

  var nodes: List[Node] = List.empty
  var nextNodeID: Int = 0

  def addNode(representedNode:Int, vehicle:Int, label:Int,nodeType:VLSNSNodeType):Node = {
    require(label >=0 && label < nbLabels)
    val n = new Node(nextNodeID, representedNode:Int, nodeType, vehicle:Int, label)
    nextNodeID += 1
    nodes = n :: nodes
    n
  }

  def newFreshLabel():Int = {
    val toReturn = nbLabels
    nbLabels = nbLabels +1
    toReturn
  }

  def finish():(Array[Node],Int) = {
    (nodes.reverse.toArray,nbLabels)
  }
}

class VLSNEdgeBuilder(nodes:Array[Node],nbLabels:Int,v:Int){
  val nbNodes = nodes.length
  val edges:Array[Array[Edge]] = Array.tabulate(nbNodes)(_ => Array.fill(nbNodes)(null))
  var fromToWithEdge:List[(Int,Int)] = List.empty
  var nextEdgeID:Int = 0

  def addEdge(from:Node, to:Node, deltaObj:Int, move:Move, vLSNMoveType: VLSNMoveType):Edge = {
    require(edges(from.nodeID)(to.nodeID) == null)
    val edge = new Edge(from:Node,to:Node, move:Move,deltaObj:Int, nextEdgeID, vLSNMoveType)
    edges(from.nodeID)(to.nodeID) = edge
    nextEdgeID += 1
    fromToWithEdge = (from.nodeID,to.nodeID) :: fromToWithEdge
    edge
  }

  def finish():VLSNGraph = {
    val edgeArray:Array[Edge] = Array.fill(nextEdgeID)(null)

    for((from,to) <- fromToWithEdge){
      val edge = edges(from)(to)
      edge.registerToNodes()
      edgeArray(edge.edgeID) = edge
    }

    new VLSNGraph(nodes,edgeArray,nbLabels,v)
  }

  override def toString: String = "EdgeBuilder( nbNodes:" + nbNodes + " nbEdges:" + nextEdgeID + ")"
}

/**
  * @param nodes
  * @param edges
  * @param nbLabels labels range from 0 to nbLabels-1
  */
class VLSNGraph(val nodes:Array[Node],val edges:Array[Edge],val nbLabels:Int, v:Int){
  val nbNodes = nodes.length
  val nbEdges = edges.length

  override def toString: String = "VLSNGraph(nbNodes:" + nbNodes + ",nbEdges:" + nbEdges +
    "\n\tnodes{\n\t\t"+ nodes.mkString("\n\t\t") + "\n\t}" +
    "\n\tedges{\n\t\t" + edges.mkString("\n\t\t") + "\n\t}" + "\n}" + "\n\n\n\n" + toDOT(List(1,2,4).map(edges(_)))

  //"C:\Program Files (x86)\Graphviz2.38\bin\neato" -Tpng  vlsnGraph.dot > a.png
  def toDOT(edgesToBold:List[Edge] = List.empty,light:Boolean = false,onlyCycles:Boolean = false):String = {
    val setOfEdgesToBold = SortedSet.empty[Int] ++ edgesToBold.map(_.edgeID)
    val setOfNodesToBold = SortedSet.empty[Int] ++ edgesToBold.map(_.from.nodeID) ++ edgesToBold.map(_.to.nodeID)
    "##Command to produce the output: \"neato -Tpng thisfile > thisfile.png\"\n" +
      "digraph VLSNGraph {\n" +
      nodes.flatMap(node => {val ofInterest = setOfNodesToBold.contains(node.nodeID)
        if(onlyCycles && !ofInterest) None else Some(node.toDOT(ofInterest))}).mkString("\t", "\n\t", "\n") +
      edges.flatMap(edge => {val ofInterest = setOfEdgesToBold.contains(edge.edgeID)
        if(onlyCycles && !ofInterest) None
        else Some(edge.toDOT(ofInterest,light))}).mkString("\t", "\n\t", "\n") +
      "\tlegend[shape=rectangle,style=filled,fillcolor=pink,color=black,label = \"" + this.statistics + "\"] ; \n" +
      "\toverlap=false\n" +
      //      "\tlabel=\"" + this.statistics + "\";\n" +
      "\tfontsize=12;\n" +
      "}"
  }

  def statistics:String = "VLSNGraph(nbNodes:" + nodes.length + " nbEdges:" + edges.length + ")"
}


class Node(val nodeID:Int, val representedNode:Int, val nodeType:VLSNSNodeType, val vehicle:Int, val label:Int){
  var incoming:List[Edge] = List.empty
  var outgoing:List[Edge] = List.empty

  override def toString: String = "Node(vlsnNode:" + nodeID + " routingNode:" + representedNode + " v:" + vehicle + " label:" + label + ")"

  def toDOT(bold:Boolean):String = {

    val fillColor = nodeType match {
      case RegularNode => "aquamarine"
      case VehicleNode => "green"
      case UnroutedNode => "crimson"
      case FictiveNode => "gold"
    }

    val dotLabel = nodeType match {
      case RegularNode => "\"node:" + representedNode + "\\nvehicle:" + vehicle + "\""
      case VehicleNode => "\"vehicle:" + vehicle + "\""
      case UnroutedNode => "\"node:" + representedNode + "\""
      case FictiveNode => "trashNode"
    }

    val lineColor = (if (bold) "blue" else "black")
    "\"" + nodeID + "\" [shape=circle,style=filled,fillcolor=" + fillColor + ",color=" + lineColor +
      ", label = " + dotLabel + "] ; "
  }
}

class Edge(val from:Node, val to:Node, val move:Move, val deltaObj:Int, val edgeID:Int, val moveType:VLSNMoveType){
  def registerToNodes(): Unit ={
    from.outgoing = this :: from.outgoing
    to.incoming = this :: to.incoming
  }

  override def toString: String = "Edge(from:" + from.nodeID + ",to:"+to.nodeID + ",deltaObj:" + deltaObj + ",type:" + moveType+ ")" + move

  def toDOTHeavy(bold:Boolean = false):String =
    "\"Edge" + edgeID + "\" [shape=rectangle,style=filled,fillcolor=gray, label=\"deltaObj:" + deltaObj +
      "\\n" + moveType +
      "\\n" + (if (move == null) "null" else move.shortString) + "\"" + (if(bold) " color=blue" else "") +"] ; " +
      from.nodeID + " -> " + "\"Edge" + edgeID + "\"" + (if(bold) "[color=blue]" else "") + ";" +
      "\"Edge" + edgeID + "\" -> " + to.nodeID + (if(bold) "[color=blue]" else "") + ";"

  def toDOTLight(bold:Boolean = false):String =
    from.nodeID + " -> " + to.nodeID + (if(bold) "[color=blue]" else "") + ";"

  def toDOT(bold:Boolean = false,light:Boolean):String =
    if(light){ toDOTLight(bold) }else toDOTHeavy(bold)
}


object VLSNGraphTest extends App{

  def buildGraph():VLSNGraph = {
    val nbNodes = 6
    val nodes = Array.tabulate(nbNodes)(nodeID =>
      new Node(nodeID, nbNodes + nodeID,VLSNSNodeType.RegularNode,nodeID,nodeID))

    val builder = new VLSNEdgeBuilder(nodes: Array[Node], nbNodes,2) //nbLAbel is set here to nbNodes

    def edge(from: Int, to: Int, gain: Int): Unit = {
      builder.addEdge(nodes(from), nodes(to), gain,  new DoNothingMove(Int.MaxValue),VLSNMoveType.SymbolicTrashToInsert)
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