package oscar.cbls.lib.invariant.graph

import oscar.cbls.CBLSIntVar
import oscar.cbls.core.computation._

import scala.collection.immutable.{SortedMap, SortedSet}

abstract class VoronoiZoneLabeling{
  def <(that:VoronoiZoneLabeling):Boolean
  def equals(that:VoronoiZoneLabeling):Boolean
}

case class VoronoiZone(centroid:Node,distance:Int) extends VoronoiZoneLabeling{
  override def <(that: VoronoiZoneLabeling): Boolean = that match{
    case Unreachable => true
    case that:VoronoiZone =>
      this.distance < that.distance || (that.distance == this.distance && this.centroid.nodeId < that.centroid.nodeId)
  }

  override def equals(that: VoronoiZoneLabeling): Boolean = that match{
    case Unreachable => false
    case that:VoronoiZone => that.distance == this.distance && this.centroid == that.centroid
  }

  def + (length:Int):VoronoiZone = VoronoiZone(centroid,distance+length)
}

case object Unreachable extends VoronoiZoneLabeling{
  override def <(that: VoronoiZoneLabeling): Boolean = false

  override def equals(that: VoronoiZoneLabeling): Boolean = that match{
    case Unreachable => true
    case that:VoronoiZone => false
  }
}

case class OutputLabeling(distance:CBLSIntVar,
                          centroid:CBLSIntVar){

  def set(l:VoronoiZoneLabeling): Unit ={
    l match{
      case Unreachable =>
        setUnreachable()

      case VoronoiZone(centroid,distance) =>
        this.centroid := centroid.nodeId
        this.distance := distance
    }
  }

  def setUnreachable(): Unit ={
    this.centroid := -1
    this.distance := 0
  }
}

/**
  *
  * @param graph a graph, this is a constant. it is a conditiona lgraph, so some edges have a Boolean proposition associated to them
  * @param openConditions the set of conditions such that the edge is considered open
  * @param centroids the centroids
  * @param trackedNodeToDistanceAndCentroidMap this is the output:
  *                                            for the nodes that require it, the distance to the closestCentroid and the distance to this centroid
  */
class VoronoiZones(graph:ConditionalGraph,
                   openConditions:ChangingSetValue,
                   centroids:ChangingSetValue,
                   trackedNodeToDistanceAndCentroidMap:SortedMap[Int,(CBLSIntVar,CBLSIntVar)])
  extends Invariant with SetNotificationTarget{

  //TODO: this invariant would divide its runtime by two in case of global checkpointing

  registerStaticAndDynamicDependency(openConditions,0)
  registerStaticAndDynamicDependency(centroids,1)

  finishInitialization()

  val trackedNodeToDistanceAndCentroid:Array[OutputLabeling] =
    Array.tabulate(graph.nbNodes)(nodeID =>
      trackedNodeToDistanceAndCentroidMap.get(nodeID) match{
        case None => null
        case Some(couple@(a,b)) =>
          a.setDefiningInvariant(this)
          b.setDefiningInvariant(this)
          OutputLabeling(a,b)
      })

  val isCentroid:Array[Boolean] = Array.fill(graph.nbNodes)(false)

  val isConditionalEdgeOpen:Array[Boolean] = Array.fill(graph.nbConditions)(false)

  def isEdgeOpen(edge: Edge): Boolean =
    edge.conditionID match {
      case None => true
      case Some(condition) => isConditionalEdgeOpen(condition)
    }

  val nodeLabeling:Array[VoronoiZoneLabeling] = Array.fill(graph.nbNodes)(Unreachable)

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]): Unit ={

    if(v ==  centroids){
      for(added <- addedValues){
        addCentroid(added)
      }
      for(removed <- removedValues){
        removeCentroid(removed)
      }
    }else if (v == openConditions){
      for(added <- addedValues){
        openEdge(added)
      }
      for(removed <- removedValues){
        closeEdge(removed)
      }
    }else{
      require(false,"got notification for not centroid and not openConditions")
    }
  }

  def addCentroid(added:Int): Unit ={


  }

  def removeCentroid(removed:Int): Unit ={

  }
  def openEdge(added:Int): Unit ={

  }
  def closeEdge(removed:Int): Unit ={

  }





  //we can only put node with an existing under-approximated distance to the target, this only needs to be checked on the source node, actually
  val nodeIDHeap = new oscar.cbls.algo.heap.BinomialHeapWithMoveInt(
    nodeID => nodeLabeling(nodeID).asInstanceOf[VoronoiZone].distance, graph.nbNodes, graph.nbNodes)

  def performLabelingFromCurrentHeap(){
    while(!nodeIDHeap.isEmpty){
      val currentNodeId: Int = nodeIDHeap.removeFirst()
      val currentNode = graph.nodes(currentNodeId)
      val currentNodeLabeling = nodeLabeling(currentNodeId).asInstanceOf[VoronoiZone]

      for(edge <- currentNode.incidentEdges if isEdgeOpen(edge)){
        val otherNode = edge.otherNode(currentNode)
        val otherNodeID = otherNode.nodeId
        val newLabelingForOtherNode = currentNodeLabeling + edge.length

        if(newLabelingForOtherNode < nodeLabeling(otherNodeID)){

          nodeLabeling(otherNodeID) = newLabelingForOtherNode

          if(trackedNodeToDistanceAndCentroid(otherNodeID) != null){
            trackedNodeToDistanceAndCentroid(otherNodeID).set(newLabelingForOtherNode)
          }

          if(nodeIDHeap.contains(otherNodeID)){
            nodeIDHeap.notifyChange(otherNodeID)
          }else{
            nodeIDHeap.insert(otherNodeID)
          }
        }
      }
    }
  }

  def loadAllCentroidsIntoHeap(centroids:Iterable[Int]): Unit ={
    for(centroid <- centroids){
      nodeIDHeap.insert(centroid)
    }
  }

  def loadExternalBoundaryIntoHeapMarkImpactedZone(closedEdge:Edge): Unit ={
    require(nodeIDHeap.isEmpty)
    require(closedEdge.length > 0)

    val nodeA = closedEdge.nodeA
    val markingA = nodeLabeling(nodeA.nodeId)
    val nodeB = closedEdge.nodeB
    val markingB = nodeLabeling(nodeB.nodeId)

    val orphanNodeOpt = (markingA,markingB) match{
      case (VoronoiZone(centroidA,dA),VoronoiZone(centroidB,dB)) if centroidA == centroidB =>
        if (dA + closedEdge.length == dB){
          //nodeB is orphan
          Some(nodeB)
        } else if (dB + closedEdge.length == dA){
          //nodeA is orphan
          Some(nodeA)
        }else{
          None
        }
      case _ => None
    }

    orphanNodeOpt match {
      case Some(orphanNode) =>
        val orphanNodeID = orphanNode.nodeId
        val orphanNodeLabeling = nodeLabeling(orphanNodeID).asInstanceOf[VoronoiZone]
        val minDistance = orphanNodeLabeling.distance
        val centroidThrough = orphanNodeLabeling.centroid

        def explore(node: Node) {
          for (edge <- node.incidentEdges if isEdgeOpen(edge)) {
            val otherNode = edge.otherNode(node)
            val otherNodeID = otherNode.nodeId

            nodeLabeling(otherNodeID) match {
              case VoronoiZone(centroid: Node, distance: Int) =>
                if (centroid == centroidThrough && distance >= minDistance) {
                  //still marking
                  nodeLabeling(otherNodeID) = Unreachable
                  if(trackedNodeToDistanceAndCentroid(otherNodeID) != null){
                    trackedNodeToDistanceAndCentroid(otherNodeID).setUnreachable()
                  }

                  explore(otherNode)
                } else if (centroid != centroidThrough) {
                  //we are at another centroid
                  if (!nodeIDHeap.contains(otherNode.nodeId)) {
                    //not stored yet, we store it
                    nodeIDHeap.insert(otherNode.nodeId)
                  }
                }
            }
          }
        }

        nodeLabeling(orphanNodeID) = Unreachable

        if(trackedNodeToDistanceAndCentroid(orphanNodeID) != null){
          trackedNodeToDistanceAndCentroid(orphanNodeID).setUnreachable()
        }

        explore(orphanNode)

      case None => //no passing through centroid, nothing to do

    }
  }

  def loadExternalBoundaryIntoHeapMarkInnerZone(removedCentroid:Node){
    require(nodeIDHeap.isEmpty)
    //performed as a DFS, non-redundant exploration, so not very costly
    def explore(node:Node){
      for(edge <- node.incidentEdges if isEdgeOpen(edge)){
        val otherNode = edge.otherNode(node)
        val otherNodeID = otherNode.nodeId

        nodeLabeling(otherNodeID) match{
          case VoronoiZone(centroid:Node,distance:Int) =>
            if (centroid == removedCentroid){

              nodeLabeling(otherNodeID) = Unreachable
              if(trackedNodeToDistanceAndCentroid(otherNodeID) != null){
                trackedNodeToDistanceAndCentroid(otherNodeID).setUnreachable()
              }

              explore(otherNode)
            } else {
              //we are at anotherNOde
              if(!nodeIDHeap.contains(otherNodeID)){
                //not stored yet, we store it
                nodeIDHeap.insert(otherNodeID)
              }
            }
          case Unreachable => ;
          //we can reach an unreacable one in case two path from the removed centroid lead to the same node
        }
      }
    }
    explore(removedCentroid)
  }
}

