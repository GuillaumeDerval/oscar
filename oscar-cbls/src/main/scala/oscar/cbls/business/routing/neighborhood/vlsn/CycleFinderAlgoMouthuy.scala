package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.algo.magicArray.MagicBoolArray

import scala.collection.mutable


class CycleFinderAlgoMouthuy(graph:VLSNGraph[Int]) extends CycleFinderAlgo{
  private val nodes:Array[Node[Int]] = graph.nodes
  private val edges:Array[Edge[Int]] = graph.edges
  private val nbNodes = nodes.length
  private val nodeRange = 0 until nbNodes

  private val isLabelOnPath = MagicBoolArray(graph.nbLabels,false)
  private val isNodeOnPath = MagicBoolArray(nbNodes,false)

  private val selectedIncomingEdges:Array[Edge[Int]] = Array.fill(nbNodes)(null)
  private val distanceToNode:Array[Int]= Array.fill(nbNodes)(Int.MaxValue)

  var isLiveNode:Array[Boolean] = null

  val isInQueue:Array[Boolean]= Array.fill(nbNodes)(false)
  val queue = new mutable.Queue[Int]()
  def enqueue(nodeID:Int): Unit ={
    if(isLiveNode(nodeID)) {
      if (!isInQueue(nodeID)) {
        queue.enqueue(nodeID)
        isInQueue(nodeID) = true
      }
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
  case class CycleFound(cycle:List[Edge[Int]]) extends MarkingResult
  case class PartialCycleFound(cycle:List[Edge[Int]],rootNode:Node[Int]) extends MarkingResult
  case class MarkingDone(duplicateLabels:Boolean) extends MarkingResult

  def markPathTo(node:Node[Int],labelDuplicates:Boolean):MarkingResult = {

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

  def extractCycle(rootNode:Node[Int]):CycleFound = {
    var currentNode = rootNode
    var toReturn:List[Edge[Int]] = List.empty
    while(true){
      val incomingEdge = selectedIncomingEdges(currentNode.nodeID)
      toReturn = incomingEdge :: toReturn
      currentNode = incomingEdge.from

      if(currentNode == rootNode){
        val sum = toReturn.map(_.deltaObj).sum
        require(sum < 0)
        return CycleFound(toReturn)
      }
    }
    throw new Error("no cycle found in extract cycle method")
  }

  def correctLabel(node:Node[Int]): MarkingResult = {
    val nodeID = node.nodeID

    isLabelOnPath.all = false
    isNodeOnPath.all = false

    require(isLabelOnPath.indicesAtTrue.isEmpty)
    require(isNodeOnPath.indicesAtTrue.isEmpty)

    markPathTo(node:Node[Int],false) match{
      case f:CycleFound =>
        return f
      case MarkingDone(duplicateLabels) if !duplicateLabels =>
        var outgoingEdges = node.outgoing
        while(outgoingEdges.nonEmpty){

          val currentEdge = outgoingEdges.head
          outgoingEdges = outgoingEdges.tail

          val toNode = currentEdge.to
          val toNodeID = toNode.nodeID
          if(isNodeOnPath(toNodeID) && distanceToNode(nodeID) - distanceToNode(toNodeID) + currentEdge.deltaObj < 0) {
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

  def searchRootedCycle(rootNode:Node[Int]):Option[CycleFound] = {

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


  override def findCycle(liveNodes:Array[Boolean]):Option[List[Edge[Int]]] = {
    isLiveNode = liveNodes
    for(rootNode <- nodes if liveNodes(rootNode.nodeID)){
      searchRootedCycle(rootNode) match{
        case None => ;
        case Some(c)  => return Some(c.cycle)
      }
    }
    return None
  }
}

