package oscar.cbls.lib.invariant.graph

import scala.collection.immutable.SortedSet


class AStar(g:ConditionalGraph,
            underApproximatingDistance:(Int,Int) => Option[Int]){

  def search(from:Node,
             to:Node,
             isConditionalEdgeOpen:Int => Boolean
            ):IncrementalDistanceResult = {

    def isEdgeOpen(edge: Edge): Boolean =
      edge.conditionID match {
        case None => true
        case Some(condition) => isConditionalEdgeOpen(condition)
      }

    if (underApproximatingDistance(from.nodeId, to.nodeId).isEmpty) {
      return NeverConnected(from: Node)
    }

    val nodeToDistance = Array.fill[Int](g.nodes.length)(Int.MaxValue)
    var reachedClosedEdges: SortedSet[Int] = SortedSet.empty

    //we can only put node with an existing under-approximated distance to the target, this only needs to be checked on the source node, actually
    val toDevelopHeap = new oscar.cbls.algo.heap.BinomialHeapWithMoveInt(
      nodeID => nodeToDistance(nodeID) + underApproximatingDistance(nodeID, to.nodeId).get,
      g.nodes.length,
      g.nodes.length - 1)

    nodeToDistance(from.nodeId) = 0
    toDevelopHeap.insert(from.nodeId)

    while (true) {

      val currentNodeId: Int = if (!toDevelopHeap.isEmpty) -1
      else toDevelopHeap.removeFirst()

      if (currentNodeId == -1 || (nodeToDistance(currentNodeId) > nodeToDistance(to.nodeId))) {
        //this is the exit code
        return extractAnswerFromFinishedSearch(
          from:Node,
          to:Node,
          _ match{
            case None => true
            case Some(c) => isConditionalEdgeOpen(c)},
          nodeToDistance:Array[Int],
          reachedClosedEdges: SortedSet[Int])
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


  private def extractAnswerFromFinishedSearch(from:Node,
                                              to:Node,
                                              isConditionalEdgeOpen:Option[Int] => Boolean,
                                              nodeToDistance:Array[Int],
                                              reachedClosedEdges: SortedSet[Int]):IncrementalDistanceResult = {

    if (nodeToDistance(to.nodeId) == Int.MaxValue) {
      // not reached
      return NotConnected(from: Node,
        to: Node,
        reachedClosedEdges)
    } else {
      //connected
      return Distance(from: Node,
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
