package oscar.cbls.algo.graph

import oscar.cbls.algo.quick.QList

import scala.collection.immutable.SortedSet

abstract sealed class RevisableDistance(from:Node,
                                        to:Node)

case class Distance(from:Node,
                    to:Node,
                    distance:Long,
                    requiredConditions:SortedSet[Int],
                    unlockingConditions:SortedSet[Int],
                    path:Option[List[Edge]]) extends RevisableDistance(from,to)

case class NeverConnected(from:Node,to:Node) extends RevisableDistance(from,to)

case class NotConnected(from:Node,
                        to:Node,
                        unlockingConditions:SortedSet[Int]) extends RevisableDistance(from,to)

/**
  *
  * @param graph
  * @param underApproximatingDistance MaxInt is where thre is no connexion
  */
class RevisableAStar(graph:ConditionalGraph,
                     underApproximatingDistance:(Int,Int) => Long){

  //TODO: speed up if we know, beside the under approximating distance, that the approximation is exact (ie: no conditional edge on the shortest path)
  //then either the search can be stopped andthe distance is just added and returned
  //or the path can be computed faster, given that we know the approximation is exact.

  private val nodeToDistance = Array.fill[Long](graph.nodes.length)(Long.MaxValue)

  def search(from:Node,
             to:Node,
             isConditionalEdgeOpen:Int => Boolean,
             includePath:Boolean
            ):RevisableDistance = {

    def isEdgeOpen(edge: Edge): Boolean =
      edge.conditionID match {
        case None => true
        case Some(condition) => isConditionalEdgeOpen(condition)
      }

    if (underApproximatingDistance(from.nodeId, to.nodeId) == Long.MaxValue) {
      return NeverConnected(from, to)
    }

    //TODO: this array might be time-consuming to allocate; store it permanently in the class for faster query time?

    var reachedClosedConditions: SortedSet[Int] = SortedSet.empty

    var reachedNodeIDs:QList[Int] = null

    //we can only put node with an existing under-approximated distance to the target, this only needs to be checked on the source node, actually
    val toDevelopHeap = new oscar.cbls.algo.heap.BinomialHeapWithMoveLong(
      nodeID => nodeToDistance(nodeID) + underApproximatingDistance(nodeID, to.nodeId),
      graph.nodes.length,
      graph.nodes.length - 1)

    val fromNodeID = from.nodeId
    nodeToDistance(fromNodeID) = 0
    reachedNodeIDs = QList(fromNodeID)
    toDevelopHeap.insert(fromNodeID)

    while (true) {

      val currentNodeId: Int = if (toDevelopHeap.isEmpty) -1
      else toDevelopHeap.removeFirst()

      if (currentNodeId == -1 || (nodeToDistance(currentNodeId) > nodeToDistance(to.nodeId))) {
        //this is the exit code
        val toReturn = extractAnswerFromFinishedSearch(
          from:Node,
          to:Node,
          _ match{
            case None => true
            case Some(c) => isConditionalEdgeOpen(c)},
          nodeToDistance:Array[Long],
          pruneReachedClosedConditions(reachedClosedConditions:SortedSet[Int],to.nodeId,nodeToDistance(to.nodeId)),
          includePath)
        resetReachedNodes(reachedNodeIDs)
        return toReturn
      }

      val currentNode = graph.nodes(currentNodeId)
      val currentNodeDistance = nodeToDistance(currentNodeId)
      for (outgoingEdge <- currentNode.incidentEdges) {
        if (isEdgeOpen(outgoingEdge)) {
          val otherNode = outgoingEdge.otherNode(currentNode)
          val otherNodeID = otherNode.nodeId

          val oldDistance = nodeToDistance(otherNodeID)
          val newDistance = currentNodeDistance + outgoingEdge.length
          if (newDistance < oldDistance) {
            nodeToDistance(otherNodeID) = newDistance
            if (toDevelopHeap.contains(otherNodeID)) {
              //Already to explore
              toDevelopHeap.notifyChange(otherNodeID)
            } else {
              reachedNodeIDs = QList(otherNodeID, reachedNodeIDs)
              toDevelopHeap.insert(otherNodeID)
            }
          }

        } else {
          //it is closed, but might be open later one
          reachedClosedConditions = reachedClosedConditions + outgoingEdge.conditionID.get
        }
      }
    }
    throw new Error("should not be reached")
  }


  def pruneReachedClosedConditions(reachedClosedConditions:SortedSet[Int],to:Int,distance:Long):SortedSet[Int] = {
    reachedClosedConditions.filter((conditionID:Int) => {
      val edge = graph.conditionToConditionalEdges(conditionID)

      val nodeAID = edge.nodeA.nodeId
      val distanceA = nodeToDistance(nodeAID)
      val nodeBID = edge.nodeB.nodeId
      val distanceB = nodeToDistance(nodeBID)

      val (minDistance, closestNodeID, farNodeID) = if (distanceA < distanceB) (distanceA, nodeAID, nodeBID) else (distanceB, nodeBID, nodeAID)

      minDistance + edge.length + underApproximatingDistance(farNodeID,to) <= distance
    })
  }


  private def resetReachedNodes(reachedNodes:QList[Int]): Unit ={
    var remainingNodeIDs = reachedNodes
    while(remainingNodeIDs != null){
      nodeToDistance(remainingNodeIDs.head) = Long.MaxValue
      remainingNodeIDs = remainingNodeIDs.tail
    }
  }

  private def extractAnswerFromFinishedSearch(from:Node,
                                              to:Node,
                                              isConditionalEdgeOpen:Option[Int] => Boolean,
                                              nodeToDistance:Array[Long],
                                              reachedClosedEdges: SortedSet[Int],
                                              includePath:Boolean):RevisableDistance = {

    if (nodeToDistance(to.nodeId) == Long.MaxValue) {
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
            nodeToDistance:Array[Long]),
        unlockingConditions = reachedClosedEdges,
        path = if(includePath) Some(extractPath(from:Node,
          to:Node,
          isConditionalEdgeOpen:Option[Int] => Boolean,
          nodeToDistance:Array[Long])) else None
      )
    }
  }

  private def extractRequiredConditions(from:Node,
                                        to:Node,
                                        isConditionalEdgeOpen:Option[Int] => Boolean,
                                        nodeToDistance:Array[Long]):SortedSet[Int] = {
    //we extract the set of conditions found on the actual path
    var toReturn: SortedSet[Int] = SortedSet.empty
    var currentNode: Node = to
    var currentDistance: Long = nodeToDistance(to.nodeId)
    while (currentNode != from) {
      for (incomingEdge <- currentNode.incidentEdges if isConditionalEdgeOpen(incomingEdge.conditionID)) {

        val newNode = incomingEdge.otherNode(currentNode)
        val newDistance = nodeToDistance(newNode.nodeId)

        if (newDistance != Long.MaxValue && newDistance + incomingEdge.length == currentDistance) {

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

  def extractPath(from:Node,
                  to:Node,
                  isConditionalEdgeOpen:Option[Int] => Boolean,
                  nodeToDistance:Array[Long]):List[Edge] = {
    //we extract the set of conditions found on the actual path
    var toReturn: List[Edge] = List.empty
    var currentNode: Node = to
    var currentDistance: Long = nodeToDistance(to.nodeId)
    while (currentNode != from) {
      for (incomingEdge <- currentNode.incidentEdges if isConditionalEdgeOpen(incomingEdge.conditionID)) {

        val newNode = incomingEdge.otherNode(currentNode)
        val newDistance = nodeToDistance(newNode.nodeId)

        if (newDistance != Long.MaxValue && newDistance + incomingEdge.length == currentDistance) {

          currentDistance = newDistance
          currentNode = newNode

          toReturn = incomingEdge :: toReturn
        }
      }
    }
    toReturn
  }
}



