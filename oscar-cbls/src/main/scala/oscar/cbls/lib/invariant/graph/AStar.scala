package oscar.cbls.lib.invariant.graph

import oscar.cbls.core.computation.{IntNotificationTarget, SetNotificationTarget}
import oscar.cbls.core.{ChangingIntValue, ChangingSetValue, IntInvariant, ValueWiseKey}

import scala.collection.immutable.SortedSet

class DistanceInConditionalGraph(graph:ConditionalGraph,
                                 from:ChangingIntValue,
                                 to:ChangingIntValue,
                                 openConditions:ChangingSetValue,
                                 distanceIfNotConnected:Int)
                                (underApproximatingDistance:(Int,Int) => Option[Int]
                                 = {val m = FloydWarshall.buildDistanceMatrix(graph,_ => true); (a,b) => m(a)(b)})
  extends IntInvariant() with SetNotificationTarget with IntNotificationTarget{

  registerStaticAndDynamicDependency(from)
  registerStaticAndDynamicDependency(to)
  registerStaticDependency(openConditions)
  private var key:ValueWiseKey = registerDynamicValueWiseDependency(openConditions)

  val aStar = new AStar(graph, underApproximatingDistance)

  var listenedValues:SortedSet[Int] = SortedSet.empty
  def setListenedValueOnValueWiseKey(newListenedValues:SortedSet[Int]): Unit ={
    val toRemoveValues = newListenedValues -- listenedValues
    toRemoveValues.foreach(key.removeFromKey)

    val toAddValues = listenedValues -- newListenedValues
    toAddValues.foreach(key.addToKey)

    listenedValues = newListenedValues
  }

  //initialize the stuff
  computeAffectAndAdjustValueWiseKey()

  def computeAffectAndAdjustValueWiseKey(){
    aStar.search(
      graph.nodes(from.value),
      graph.nodes(to.value),
      {val o = openConditions.value; condition => o contains condition})
    match{
      case Distance(from:Node, to:Node,distance:Int, requiredConditions:SortedSet[Int], unlockingConditions:SortedSet[Int]) =>
        setListenedValueOnValueWiseKey(requiredConditions ++ unlockingConditions)

        this := distance

      case NeverConnected(from:Node,to:Node) =>
      //will only happen once at startup

        key.performRemove()
        key = null

        this := distanceIfNotConnected

      case NotConnected(from:Node, to:Node, unlockingConditions:SortedSet[Int]) =>
        setListenedValueOnValueWiseKey(unlockingConditions)

        this := distanceIfNotConnected
    }
  }

  override def notifySetChanges(v: ChangingSetValue,
                                d: Int,
                                addedValues: Iterable[Int],
                                removedValues: Iterable[Int],
                                oldValue: SortedSet[Int],
                                newValue: SortedSet[Int]): Unit = {
    scheduleForPropagation()
  }

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit = {
    scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    computeAffectAndAdjustValueWiseKey()
  }
}

abstract sealed class ConditionalDistance(from:Node,
                                          to:Node)

case class Distance(from:Node,
                    to:Node,
                    distance:Int,
                    requiredConditions:Set[Int],
                    unlockingConditions:Set[Int]) extends ConditionalDistance(from,to)

case class NeverConnected(from:Node,to:Node) extends ConditionalDistance(from,to)

case class NotConnected(from:Node,
                        to:Node,
                        unlockingConditions:Set[Int]) extends ConditionalDistance(from,to)

class AStar(g:ConditionalGraph,
            underApproximatingDistance:(Int,Int) => Option[Int]){

  def search(from:Node,
             to:Node,
             isConditionalEdgeOpen:Int => Boolean
            ):ConditionalDistance = {

    def isEdgeOpen(edge: Edge): Boolean =
      edge.conditionID match {
        case None => true
        case Some(condition) => isConditionalEdgeOpen(condition)
      }

    if (underApproximatingDistance(from.nodeId, to.nodeId).isEmpty) {
      return NeverConnected(from, to)
    }

    //TODO: this array might be time-consuming to allocate; store it permanently in the class for faster query time?
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
                                              reachedClosedEdges: SortedSet[Int]):ConditionalDistance = {

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



