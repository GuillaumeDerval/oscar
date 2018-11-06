package oscar.cbls.algo.graph

import oscar.cbls.algo.quick.QList

import scala.collection.immutable.SortedSet

abstract sealed class RevisableDistance(from:Node,
                                        to:Node)

case class Distance(from:Node,
                    to:Node,
                    distance:Int,
                    requiredConditions:Set[Int],
                    unlockingConditions:Set[Int]) extends RevisableDistance(from,to)

case class NeverConnected(from:Node,to:Node) extends RevisableDistance(from,to)

case class NotConnected(from:Node,
                        to:Node,
                        unlockingConditions:Set[Int]) extends RevisableDistance(from,to)

class RevisableAStar(g:ConditionalGraph,
                     underApproximatingDistance:(Int,Int) => Option[Int]){

  private val nodeToDistance = Array.fill[Int](g.nodes.length)(Int.MaxValue)

  def search(from:Node,
             to:Node,
             isConditionalEdgeOpen:Int => Boolean
            ):RevisableDistance = {

    def isEdgeOpen(edge: Edge): Boolean =
      edge.conditionID match {
        case None => true
        case Some(condition) => isConditionalEdgeOpen(condition)
      }

    if (underApproximatingDistance(from.nodeId, to.nodeId).isEmpty) {
      return NeverConnected(from, to)
    }

    //TODO: this array might be time-consuming to allocate; store it permanently in the class for faster query time?

    var reachedClosedEdges: SortedSet[Int] = SortedSet.empty

    var reachedNodeIDs:QList[Int] = null

    //we can only put node with an existing under-approximated distance to the target, this only needs to be checked on the source node, actually
    val toDevelopHeap = new oscar.cbls.algo.heap.BinomialHeapWithMoveInt(
      nodeID => nodeToDistance(nodeID) + underApproximatingDistance(nodeID, to.nodeId).get,
      g.nodes.length,
      g.nodes.length - 1)

    val fromNodeID = from.nodeId
    nodeToDistance(fromNodeID) = 0
    reachedNodeIDs = QList(fromNodeID)
    toDevelopHeap.insert(fromNodeID)

    while (true) {

      val currentNodeId: Int = if (!toDevelopHeap.isEmpty) -1
      else toDevelopHeap.removeFirst()

      if (currentNodeId == -1 || (nodeToDistance(currentNodeId) > nodeToDistance(to.nodeId))) {
        //this is the exit code
        val toReturn = extractAnswerFromFinishedSearch(
          from:Node,
          to:Node,
          _ match{
            case None => true
            case Some(c) => isConditionalEdgeOpen(c)},
          nodeToDistance:Array[Int],
          reachedClosedEdges: SortedSet[Int])
        resetReachedNodes(reachedNodeIDs)
        return toReturn
      }

      val currentNode = g.nodes(currentNodeId)
      val currentNodeDistance = nodeToDistance(currentNode.nodeId)
      for (outgoingEdge <- currentNode.incidentEdges) {
        if (isEdgeOpen(outgoingEdge)) {
          val otherNode = outgoingEdge.otherNode(currentNode)
          if (toDevelopHeap.contains(otherNode.nodeId)) {
            val oldDistance = nodeToDistance(otherNode.nodeId)
            val newDistance = currentNodeDistance + outgoingEdge.length
            if (newDistance < oldDistance) {
              nodeToDistance(otherNode.nodeId) = newDistance
              toDevelopHeap.notifyChange(otherNode.nodeId)
            }
          } else {
            val newDistance = currentNodeDistance + outgoingEdge.length
            nodeToDistance(otherNode.nodeId) = newDistance
            reachedNodeIDs = QList(otherNode.nodeId,reachedNodeIDs)
            toDevelopHeap.insert(otherNode.nodeId)
          }
        } else {
          //it is closed, but might be open later one
          reachedClosedEdges = reachedClosedEdges + outgoingEdge.conditionID.get
        }
      }
    }
    throw new Error("should not be reached")
  }

  private def resetReachedNodes(reachedNodes:QList[Int]): Unit ={
    var remainingNodeIDs = reachedNodes
    while(remainingNodeIDs != null){
      nodeToDistance(remainingNodeIDs.head) = Int.MaxValue
      remainingNodeIDs = remainingNodeIDs.tail
    }
  }

  private def extractAnswerFromFinishedSearch(from:Node,
                                              to:Node,
                                              isConditionalEdgeOpen:Option[Int] => Boolean,
                                              nodeToDistance:Array[Int],
                                              reachedClosedEdges: SortedSet[Int]):RevisableDistance = {

    if (nodeToDistance(to.nodeId) == Int.MaxValue) {
      // not reached
      NotConnected(
        from: Node,
        to: Node,
        reachedClosedEdges)
    } else {
      //connected
      Distance(
        from: Node,
        to: Node,
        distance = nodeToDistance(to.nodeId),
        requiredConditions =
          extractRequiredConditions(
            from:Node,
            to:Node,
            isConditionalEdgeOpen:Option[Int] => Boolean,
            nodeToDistance:Array[Int]),
        unlockingConditions = reachedClosedEdges
      )
    }
  }

  private def extractRequiredConditions(from:Node,
                                        to:Node,
                                        isConditionalEdgeOpen:Option[Int] => Boolean,
                                        nodeToDistance:Array[Int]):SortedSet[Int] = {
    //we extract the set of conditions found on the actual path
    var toReturn: SortedSet[Int] = SortedSet.empty
    var currentNode: Node = to
    var currentDistance: Int = nodeToDistance(to.nodeId)
    while (currentNode != from) {
      for (incomingEdge <- currentNode.incidentEdges if isConditionalEdgeOpen(incomingEdge.conditionID)) {

        val newNode = incomingEdge.otherNode(currentNode)
        val newDistance = nodeToDistance(newNode.nodeId)

        if (newDistance != Int.MaxValue && newDistance + incomingEdge.length == currentDistance) {

          currentDistance = newDistance
          currentNode = newNode

          incomingEdge.conditionID match {
            case Some(c) => toReturn = toReturn + c
            case _ => ;
          }
        }
      }
    }
    toReturn
  }
}



