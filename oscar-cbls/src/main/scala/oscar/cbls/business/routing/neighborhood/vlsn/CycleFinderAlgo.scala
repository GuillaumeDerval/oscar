package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.algo.magicArray.MagicBoolArray

import scala.collection.immutable.SortedSet
import scala.collection.mutable

class CycleFinderAlgoDFS(graph:VLSNGraph,pruneOnReachability:Boolean){
  private val nodes:Array[Node] = graph.nodes
  private val nbNodes = nodes.length
  private val nbLabels = graph.nbLabels

  def findCycle():Option[List[Edge]] = {

    val reachabilityMatrix = if(pruneOnReachability){
      new ReacheabilityFloydWarshall(graph:VLSNGraph).buildRechabilityMatrix()
    }else null

    //MatrixTools.printBoolMatrix(reachabilityMatrix)

    val isNodeReached = Array.fill(nbNodes)(false)
    val isLabelReached = Array.fill(nbLabels)(false)
    var rootNode:Node = null
    var rooNodeID:Int = -1

    def dfsExplore(node:Node,summedDelta:Int):Option[List[Edge]] ={
      var outgoingEdges = node.outgoing

      while(outgoingEdges != Nil){
        val currentEdge = outgoingEdges.head
        outgoingEdges = outgoingEdges.tail

        val currentEdgeDelta = currentEdge.deltaObj
        val newSummedDelta = currentEdgeDelta + summedDelta

        val targetNode = currentEdge.to
        val targetNodeID = targetNode.nodeID

        if(targetNode == rootNode){
          if (newSummedDelta <0) {
            //we just found a negative cycle
            return Some(List(currentEdge))
          } //else e found a cycle, but it is not negative
        }else if (!pruneOnReachability || reachabilityMatrix(targetNodeID)(rooNodeID)){

          val targetLabel = targetNode.label

          if (! isNodeReached(targetNodeID)
            && !isLabelReached(targetLabel)) {
            //this is a new node, it has a label that has not been reached yet
            //mark and recurse
            isLabelReached(targetLabel) = true
            isNodeReached(targetNodeID) = true
            dfsExplore(targetNode, newSummedDelta) match {
              case None => ;
              case Some(l) => return Some(currentEdge :: l)
            }
            isLabelReached(targetLabel) = false
            isNodeReached(targetNodeID) = false
          }
        }
      }
      None
    }

    for(n <- nodes){
      rootNode = n
      rooNodeID = n.nodeID
      dfsExplore(rootNode,0) match{
        case None => ;
        case a => return a
      }
    }
    None
  }
}

class CycleFinderAlgoMouthy(graph:VLSNGraph){
  private val nodes:Array[Node] = graph.nodes
  private val edges:Array[Edge] = graph.edges
  private val nbNodes = nodes.length
  private val nodeRange = 0 until nbNodes

  private val isLabelOnPath = MagicBoolArray(graph.nbLabels,false)
  private val isNodeOnPath = MagicBoolArray(nbNodes,false)

  private val selectedIncomingEdges:Array[Edge] = Array.fill(nbNodes)(null)
  private val distanceToNode:Array[Int]= Array.fill(nbNodes)(Int.MaxValue)

  val isInQueue:Array[Boolean]= Array.fill(nbNodes)(false)
  val queue = new mutable.Queue[Int]()
  def enqueue(nodeID:Int): Unit ={
    if(!isInQueue(nodeID)) {
      queue.enqueue(nodeID)
      isInQueue(nodeID) = true
    }
  }

  def dequeue():Option[Int] = {
    if(queue.isEmpty){
      None
    }else{
      val i = queue.dequeue()
      isInQueue(i) = false
      Some(i)
    }
  }

  abstract sealed class MarkingResult
  case class CycleFound(cycle:List[Edge]) extends MarkingResult
  case class PartialCycleFound(cycle:List[Edge],rootNode:Node) extends MarkingResult
  case class MarkingDone(duplicateLabels:Boolean) extends MarkingResult

  def markPathTo(node:Node,labelDuplicates:Boolean):MarkingResult = {

    val nodeID = node.nodeID
    if(isNodeOnPath(nodeID)){
      return new PartialCycleFound(List.empty,node)
    }
    isNodeOnPath(nodeID) = true

    val label = node.label
    val newLabelDuplicates = labelDuplicates || isLabelOnPath(label)
    isLabelOnPath(label) = true

    val incomingEdge = selectedIncomingEdges(nodeID)
    if(incomingEdge == null) return MarkingDone(labelDuplicates)

    val previousNode = incomingEdge.from

    markPathTo(previousNode,newLabelDuplicates) match{
      case f:CycleFound => f
      case d:MarkingDone => d
      case PartialCycleFound(l,root) =>
        if(root == node) CycleFound(incomingEdge :: l)
        else PartialCycleFound(incomingEdge :: l, root)
    }
  }

  def extractCycle(rootNode:Node):CycleFound = {
    var currentNode = rootNode
    var toReturn:List[Edge] = List.empty
    while(true){
      val incomingEdge = selectedIncomingEdges(currentNode.nodeID)
      toReturn = incomingEdge :: toReturn
      currentNode = incomingEdge.from

      if(currentNode == rootNode){
        return CycleFound(toReturn)
      }
    }
    throw new Error("no cycle found in extract cycle method")
  }

  def correctLabel(node:Node): MarkingResult = {
    val nodeID = node.nodeID

    isLabelOnPath.all = false
    isNodeOnPath.all = false


    require(isLabelOnPath.indicesAtTrue.isEmpty)
    require(isNodeOnPath.indicesAtTrue.isEmpty)

    markPathTo(node:Node,false) match{
      case f:CycleFound => return f
      case MarkingDone(duplicateLabels) if !duplicateLabels =>
        var outgoingEdges = node.outgoing
        while(outgoingEdges.nonEmpty){

          val currentEdge = outgoingEdges.head
          outgoingEdges = outgoingEdges.tail

          val toNode = currentEdge.to
          val toNodeID = toNode.nodeID
          if(isNodeOnPath(toNodeID)) {
            //we found a negative cycle
            selectedIncomingEdges(toNodeID) = currentEdge
            return extractCycle(toNode)
          }

          //TODO: not sure about the condition...
          if(!isLabelOnPath(toNode.label)) {
            val oldDistance = distanceToNode(toNodeID)
            val newDistance = distanceToNode(nodeID) + currentEdge.deltaObj
            if(newDistance < oldDistance && newDistance < 0){
              distanceToNode(toNodeID) = newDistance
              selectedIncomingEdges(toNodeID) = currentEdge
              enqueue(toNodeID)
            }
          }
        }

        MarkingDone(true)
      case x => {
        MarkingDone(true)
      }
    }
  }

  def searchRootedCycle(rootNode:Node):Option[CycleFound] = {

    for(nodeID <- nodeRange) {
      selectedIncomingEdges(nodeID) = null
      distanceToNode(nodeID) = Int.MaxValue
    }

    val rootNodeID = rootNode.nodeID
    enqueue(rootNodeID)
    distanceToNode(rootNodeID) = 0

    while(true){
      dequeue() match{
        case None =>
          return None
        case Some(nodeID) =>
          correctLabel(nodes(nodeID)) match{
            case f:CycleFound => {
              return Some(f)
            }
            case _ => ;
          }
      }
    }

    None
  }

  def findCycle():Option[List[Edge]] = {
    for(rootNode <- nodes){
      searchRootedCycle(rootNode) match{
        case None => ;
        case Some(c)  => return Some(c.cycle)
      }
    }
    return None
  }
}


/**
  * finds negative cycle.
  * does not care about labels
  * complete O(n^^3)
  * @param graph
  */
class CycleFinderAlgoFloydNoLabel(graph:VLSNGraph){
  private val nodes:Array[Node] = graph.nodes
  private val edges:Array[Edge] = graph.edges
  private val nbNodes = nodes.length
  private val nodeRange = 0 until nbNodes

  def findCycle():Option[List[Edge]] = {
    //we use the floyd warshall algo
    val (adjacencyMatrix,selectedEdges) = buildFloydMatrices()
    runFloydWarshallReturnNegCycleIfAny(adjacencyMatrix,selectedEdges)
  }

  def printDistanceMatrix(m:Array[Array[Int]]): Unit ={
    for(l <- m){
      println(l.mkString("\t"))
    }
  }

  private def buildFloydMatrices(): (Array[Array[Int]],Array[Array[Edge]]) = {
    val adjacencyMatrix:Array[Array[Int]] = Array.tabulate(nbNodes)(_ => Array.fill(nbNodes)(Int.MaxValue))
    val selectedEdges:Array[Array[Edge]] = Array.tabulate(nbNodes)(_ => Array.fill(nbNodes)(null))

    for(node <- nodes.indices){
      adjacencyMatrix(node)(node) = 0
    }

    for(edge <- edges){
      require(adjacencyMatrix(edge.from.nodeID)(edge.to.nodeID) == Int.MaxValue)
      adjacencyMatrix(edge.from.nodeID)(edge.to.nodeID) = edge.deltaObj
      selectedEdges(edge.from.nodeID)(edge.to.nodeID) = edge
    }

    (adjacencyMatrix,selectedEdges)
  }

  private def runFloydWarshallReturnNegCycleIfAny(adjacencyMatrix : Array[Array[Int]],
                                                  selectedEdges:Array[Array[Edge]],
                                                  liveNodes:Iterable[Int] = nodeRange):Option[List[Edge]] = {
    for (k <- liveNodes) {
      for (i <- liveNodes) {
        for (j <- liveNodes) {
          val oldValue = adjacencyMatrix (i)(j)
          val newValue =
            if (adjacencyMatrix (i)(k) == Int.MaxValue || adjacencyMatrix(k)(j) == Int.MaxValue) Int.MaxValue
            else adjacencyMatrix (i)(k) +  adjacencyMatrix(k)(j)

          if (newValue < oldValue){
            adjacencyMatrix(i)(j) = newValue
            selectedEdges(i)(j) = selectedEdges(i)(k)
          }

          if(i == j && newValue < 0) {
            //we have a negative cycle on node i (==j)
            return Some(findCycleInFloydResult(adjacencyMatrix: Array[Array[Int]], selectedEdges: Array[Array[Edge]], j))
          }
        }
      }
    }
    None
  }

  private def findCycleInFloydResult(distanceMatrix:Array[Array[Int]],selectedEdges:Array[Array[Edge]], rootNode:Int):List[Edge] = {
    var currentEdge = selectedEdges(rootNode)(rootNode)
    var toReturn:List[Edge] = List(currentEdge)
    while(currentEdge.to.nodeID != rootNode){
      currentEdge = selectedEdges(currentEdge.to.nodeID)(rootNode)
      toReturn = currentEdge :: toReturn
    }
    toReturn.reverse
  }
}


object MatrixTools{
  def printBoolMatrix(m:Array[Array[Boolean]]): Unit ={
    for(l <- m){
      println(l.mkString("\t"))
    }
  }
}

class ReacheabilityFloydWarshall(graph:VLSNGraph){
  private val nodes:Array[Node] = graph.nodes
  private val edges:Array[Edge] = graph.edges
  private val nbNodes = nodes.length
  private val nodeRange = 0 until nbNodes


  def buildRechabilityMatrix():Array[Array[Boolean]] = {
    val adjacencyMatrix = buildFloydMatrices()
    saturateAdjacencyMatrixToReacheabilityMatrix(adjacencyMatrix)
    adjacencyMatrix
  }

  private def buildFloydMatrices(): Array[Array[Boolean]] = {
    val adjacencyMatrix:Array[Array[Boolean]] = Array.tabulate(nbNodes)(_ => Array.fill(nbNodes)(false))

    for(node <- nodes.indices){
      adjacencyMatrix(node)(node) = true
    }

    for(edge <- edges){
      adjacencyMatrix(edge.from.nodeID)(edge.to.nodeID) = true
    }

    adjacencyMatrix
  }

  private def saturateAdjacencyMatrixToReacheabilityMatrix(adjacencyMatrix : Array[Array[Boolean]]):Unit = {
    for (k <- nodeRange) {
      for (i <- nodeRange) {
        for (j <- nodeRange) {
          adjacencyMatrix(i)(j) = adjacencyMatrix (i)(j) || adjacencyMatrix (i)(k) ||  adjacencyMatrix(k)(j)
        }
      }
    }
  }
}

object CycleFinderAlgoTest extends App{
  val graph = VLSNGraphTest.buildGraph()
  println(graph)

  println("starting DFS")
  val cycle = new CycleFinderAlgoDFS(graph,false).findCycle()
  println("done DFS")
  println(cycle)


  println("starting Moutuy")
  val cycle2 = new CycleFinderAlgoMouthy(graph:VLSNGraph).findCycle()
  println("done Moutuy")
  println("cycle found: " + cycle2)

//  println(graph.toDOT(SortedSet.empty[Int] ++ cycle.get.map(_.edgeID)))
}
