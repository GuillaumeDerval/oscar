package oscar.cbls.lib.invariant.graph

import oscar.cbls
import oscar.cbls.algo.graph._
import oscar.cbls.algo.quick.QList
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.set.SetMap
import oscar.cbls.{CBLSIntVar, Domain, SetValue}

import scala.collection.immutable.{SortedMap, SortedSet}


object VoronoiZones{
  def apply(graph:ConditionalGraph,
            graphDiameterOverApprox:Long,
            openConditions:SetValue,
            centroids:SetValue,
            trackedNodes:Iterable[Long],
            m:Store,
            defaultDistanceForUnreachableNodes:Long):VoronoiZones = {

    val trackedNodeToDistanceAndCentroid = SortedMap.empty[Long,(CBLSIntVar,CBLSIntVar)] ++ trackedNodes.map(nodeID =>
      nodeID -> (CBLSIntVar(m, 0, Domain(0L,(defaultDistanceForUnreachableNodes max graphDiameterOverApprox)), "distanceToClosestCentroid_Node" + nodeID),
        CBLSIntVar(m, 0, Domain(-1L , centroids.max), "closestCentroidToNode" + nodeID))
    )

    new VoronoiZones(graph,
      openConditions,
      centroids,
      trackedNodeToDistanceAndCentroid,
      defaultDistanceForUnreachableNodes:Long)
  }

  def orphanNodes(v:VoronoiZones):ChangingSetValue = {
    //TODO: embed this into the VoronoiVone invariant to have better runtime?
    val idToNodeAndCentroid:Array[(Int,IntValue)] = v.trackedNodeToDistanceAndCentroidMap.toList.map({case (id,(_,centroid)) => (cbls.longToInt(id),centroid)}).toArray

    SetMap(
      Filter(
        idToNodeAndCentroid.map(_._2),
        _!= -1),
      (nodeID:Long) => idToNodeAndCentroid(cbls.longToInt(nodeID))._1,
      Domain.setToDomain(idToNodeAndCentroid.map(_._1:Long).toSet))
  }
}

/**
  *
  * @param graph a graph, this is a constant. it is a conditional graph, so some edges have
  *              a Boolean proposition associated to them
  * @param openConditions the set of conditions such that the edge is considered open
  * @param centroids the centroids
  * @param trackedNodeToDistanceAndCentroidMap this is the output:
  *                                            for the nodes that require it,
  *                                            the distance to the closest centroid
  *                                            and the centroid
  *
  */
class VoronoiZones(graph:ConditionalGraph,
                   openConditions:SetValue,
                   centroids:SetValue,
                   val trackedNodeToDistanceAndCentroidMap:SortedMap[Long,(CBLSIntVar,CBLSIntVar)],
                   defaultDistanceForUnreachableNodes:Long)
  extends Invariant with SetNotificationTarget {


  //TODO: maxDistanceToCentroid:Int
  require(openConditions != centroids, "something absurd in the voronoi zone declaration")

  //this condition is needed because we use the distance to unmark the voronoi zones whe na conditional edge is closed
  require(graph.conditionToConditionalEdges.forall(_.length >0),"all conditional edges should have length >0")

  case class OutputLabeling(distance:CBLSIntVar,
                            centroid:CBLSIntVar){

    def set(l:ClosestCentroidLabeling): Unit ={
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
      this.distance := defaultDistanceForUnreachableNodes
    }

    def checkEqual(l:ClosestCentroidLabeling): Unit ={
      l match{
        case Unreachable =>
          require(this.centroid.value == -1)
          require(this.distance.value == defaultDistanceForUnreachableNodes)

        case VoronoiZone(centroid,distance) =>
          require(this.centroid.value == centroid.nodeId)
          require(this.distance.value == distance)
      }
    }
  }

  //TODO: this invariant would divide its runtime by two in case of global checkpointing

  registerStaticAndDynamicDependency(openConditions)
  registerStaticAndDynamicDependency(centroids)

  finishInitialization()

  private val trackedNodeToDistanceAndCentroid: Array[OutputLabeling] =
    Array.tabulate(graph.nbNodes)(nodeID =>
      trackedNodeToDistanceAndCentroidMap.get(nodeID) match {
        case None => null
        case Some((distanceVar, centroidVar)) =>
          distanceVar.setDefiningInvariant(this)
          centroidVar.setDefiningInvariant(this)
          OutputLabeling(distance=distanceVar,centroid = centroidVar)
      })

  private val isConditionalEdgeOpen: Array[Boolean] = Array.fill(graph.nbConditions)(false)

  for(c <- openConditions.value){
    isConditionalEdgeOpen(cbls.longToInt(c)) = true
  }

  private def isEdgeOpen(edge: Edge): Boolean =
    edge.conditionID match {
      case None => true
      case Some(condition) => isConditionalEdgeOpen(condition)
    }

  private val nodeLabeling: Array[ClosestCentroidLabeling] = Array.fill(graph.nbNodes)(Unreachable)

  private def labelNode(nodeID:Int,label:ClosestCentroidLabeling): Unit ={
    nodeLabeling(nodeID) = label
    if(trackedNodeToDistanceAndCentroid(nodeID) != null){
      trackedNodeToDistanceAndCentroid(nodeID).set(label)
    }
  }

  def centroidToReachedConditions:SortedMap[Int,SortedSet[Int]] = {
    val a = conditionToClosestCentroid
    var toReturn:SortedMap[Int,SortedSet[Int]] = SortedMap.empty
    for(condition <- a.indices){
      a(condition) match{
        case None => ;
        case Some(centroid) =>
          toReturn = toReturn + (centroid->(toReturn.getOrElse(centroid,SortedSet.empty[Int]) + condition))
      }
    }
    toReturn
  }

  def conditionToClosestCentroid:Array[Option[Int]] = {
    Array.tabulate(graph.nbConditions)(c => {
      val conditionalEdge = graph.conditionToConditionalEdges(c)
      (nodeLabeling(conditionalEdge.nodeA.nodeId) min nodeLabeling(conditionalEdge.nodeB.nodeId)) match{
        case VoronoiZone(centroid,_) => Some(centroid.nodeId)
        case _ => None
      }
    }
    )
  }

  def spanningTree(nodes:QList[Node]):QList[Edge] = {
    require(!isScheduled,"cannot invoke spanning tree when Voronoi is not up to date!")
    var acc:QList[Edge] = null

    var toDevelop = nodes
    while(toDevelop!=null){
      val p = pathToCentroid(toDevelop.head)
      toDevelop = toDevelop.tail

      p match{
        case None => ;
        case Some(l) =>
          var toEnqueue = l
          while(toEnqueue != null) {
            acc = QList(toEnqueue.head, acc)
            toEnqueue = toEnqueue.tail
          }
      }
    }
    acc
  }

  def pathToCentroid(node:Node):Option[QList[Edge]] = {
    require(!isScheduled,"cannot invoke path to centroid when Voronoi is not up to date!")
    def pathToExistingCentroid(node:Node,zone:VoronoiZone):QList[Edge] = {
      if(node == zone.centroid){
        null
      }else{
        for (edge <- node.incidentEdges if isEdgeOpen(edge)) {
          val otherNode = edge.otherNode(node)
          nodeLabeling(otherNode.nodeId) match {
            case z@VoronoiZone(centroid: Node, distance: Long) =>
              if (centroid == zone.centroid && distance + edge.length == zone.distance) {
                //step backward
                return QList(edge,pathToExistingCentroid(otherNode, z))
              }
            case _ => ;
          }
        }
        throw new Error("should not happen")
      }
    }

    nodeLabeling(node.nodeId) match {
      case Unreachable =>
        None
      case z:VoronoiZone =>
        Some(pathToExistingCentroid(node,z))
    }
  }

  override def notifySetChanges(v: ChangingSetValue,
                                d: Int,
                                addedValues: Iterable[Long],
                                removedValues: Iterable[Long],
                                oldValue: SortedSet[Long],
                                newValue: SortedSet[Long]): Unit = {

    if (v == centroids) {
      //println("change on centroids(addedValues:" + addedValues + " removedValues:" + removedValues)
      for (added <- addedValues) {
        val addedInt = cbls.longToInt(added)
        labelNode(addedInt,VoronoiZone(graph.nodes(addedInt),0))
        loadOrCorrectNodeIDIntoHeap(addedInt)
      }
      for (removed <- removedValues) {
        loadExternalBoundaryIntoHeapMarkInnerZone(graph.nodes(cbls.longToInt(removed)))
      }
    } else if (v == openConditions) {
      //opening or closing edges
      //println("changed open conditions(addedValues:" + addedValues + " removedValues:" + removedValues + " oldValue:" + oldValue + " newValue:" + newValue)
      for (added <- addedValues) {
        val addedInt = cbls.longToInt(added)
        assert(isConditionalEdgeOpen(addedInt))
        //if the edge is not reachable, no need to load it.
        isConditionalEdgeOpen(addedInt) = true
        assert(graph.conditionToConditionalEdges(addedInt).conditionID contains  addedInt)
        loadEdgeExtremitiesIntoHeapIfReachable(graph.conditionToConditionalEdges(addedInt))
      }
      for (removed <- removedValues) {
        val removedInt = cbls.longToInt(removed)
        assert(isConditionalEdgeOpen(removedInt))
        isConditionalEdgeOpen(removedInt) = false
        assert(graph.conditionToConditionalEdges(removedInt).conditionID contains removedInt)
        loadExternalBoundaryIntoHeapMarkImpactedZone(graph.conditionToConditionalEdges(removedInt))
      }
    } else {
      require(false, "got notification for not centroid and not openConditions")
    }
    scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    //println("START perform propagation")
    performLabelingFromCurrentHeap()
    //println("END perform propagation")
  }

  //we can only put node with an existing under-approximated distance to the target, this only needs
  // to be checked on the source node, actually
  private val nodeIDHeap = new oscar.cbls.algo.heap.BinomialHeapWithMoveLong(
    nodeID => nodeLabeling(cbls.longToInt(nodeID)).asInstanceOf[VoronoiZone].distance, graph.nbNodes, graph.nbNodes)

  private def performLabelingFromCurrentHeap() {
    while (!nodeIDHeap.isEmpty) {
      val currentNodeId: Int = nodeIDHeap.removeFirst()
      val currentNode = graph.nodes(currentNodeId)
      val currentNodeLabeling = nodeLabeling(currentNodeId).asInstanceOf[VoronoiZone]

      var l = currentNode.incidentEdges
      while(l != null){
        val edge = l.head
        l = l.tail
        if (isEdgeOpen(edge)){
          val otherNode = edge.otherNode(currentNode)
          val otherNodeID = otherNode.nodeId
          val newLabelingForOtherNode = currentNodeLabeling + edge.length

          if (newLabelingForOtherNode < nodeLabeling(otherNodeID)) {

            labelNode(otherNodeID,newLabelingForOtherNode)
            loadOrCorrectNodeIntoHeap(otherNode)
          }
        }
      }
    }
  }

  markAllNodesUnreachable()
  loadCentroidsIntoHeap(this.centroids.value)
  performLabelingFromCurrentHeap()

  private def markAllNodesUnreachable(): Unit = {
    for (node <- graph.nodes) {
      markNodeUnreachableAndRemoveFromHeapIfPresent(node:Node)
    }
  }


  private def markNodeUnreachableAndRemoveFromHeapIfPresent(node:Node): Unit ={
    val nodeID = node.nodeId
    labelNode(nodeID,Unreachable)
    nodeIDHeap.deleteIfPresent(nodeID)
  }

  private def loadCentroidsIntoHeap(centroids: Iterable[Long]): Unit = {
    for (centroid <- centroids) {
      val centroidInt = cbls.longToInt(centroid)
      if (!nodeIDHeap.contains(centroidInt)) {
        loadOrCorrectNodeIDIntoHeap(centroidInt)
      }
    }
  }

  private def loadOrCorrectNodeIDIntoHeap(nodeID: Int): Unit = {
    if (nodeIDHeap.contains(nodeID)) {
      nodeIDHeap.notifyChange(nodeID)
    } else {
      //not stored yet, we store it
      nodeIDHeap.insert(nodeID)
    }
  }

  private def loadOrCorrectNodeIntoHeap(node:Node): Unit ={
    loadOrCorrectNodeIDIntoHeap(node.nodeId)
  }

  private def loadEdgeExtremitiesIntoHeapIfReachable(edge:Edge): Unit ={
    nodeLabeling(edge.nodeA.nodeId) match{
      case _:VoronoiZone => loadOrCorrectNodeIntoHeap(edge.nodeA)
      case _ => ;
    }

    nodeLabeling(edge.nodeB.nodeId) match{
      case _:VoronoiZone => loadOrCorrectNodeIntoHeap(edge.nodeB)
      case _ => ;
    }
  }

  private def loadEdgeExtremitiesIntoHeap(edge:Edge): Unit ={
    loadOrCorrectNodeIntoHeap(edge.nodeA)
    loadOrCorrectNodeIntoHeap(edge.nodeB)
  }

  private def loadExternalBoundaryIntoHeapMarkImpactedZone(closedEdge:Edge): Unit ={
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

        markNodeUnreachableAndRemoveFromHeapIfPresent(orphanNode)


        //we usse an iteratie approach here with explicit front
        // because a recursive approach did lead to stack overflow in large graphs.
        var toDevelop: QList[Node] = QList(orphanNode)

        while (toDevelop != null) {
          val currentNode = toDevelop.head
          toDevelop = toDevelop.tail

          for (edge <- currentNode.incidentEdges if isEdgeOpen(edge)) {
            val otherNode = edge.otherNode(currentNode)
            val otherNodeID = otherNode.nodeId

            nodeLabeling(otherNodeID) match {
              case VoronoiZone(centroid: Node, distance: Long) =>
                if (centroid == centroidThrough && distance >= minDistance) {
                  //still marking
                  markNodeUnreachableAndRemoveFromHeapIfPresent(otherNode)
                  toDevelop = QList(otherNode, toDevelop)
                } else {
                  //we are at another centroid, or found a path from the same centroid that does not take the closed edge.
                  loadOrCorrectNodeIDIntoHeap(otherNodeID)
                }
              case x => ; //it can be unreachable, no worries
            }
          }
        }

      case None => //no passing through centroid, nothing to do

    }
  }

  private def loadExternalBoundaryIntoHeapMarkInnerZone(removedCentroid:Node){
    //performed as a DFS, non-redundant exploration, so not very costly
    //TODO: try an explicit tack to replace the recursion since there is a risk of stack overflow in large graphs.
    def explore(node:Node){
      for(edge <- node.incidentEdges if isEdgeOpen(edge)){
        val otherNode = edge.otherNode(node)
        val otherNodeID = otherNode.nodeId

        nodeLabeling(otherNodeID) match{
          case VoronoiZone(centroid:Node,distance:Long) =>
            if (centroid == removedCentroid){

              markNodeUnreachableAndRemoveFromHeapIfPresent(otherNode)

              explore(otherNode)
            } else {
              //we are at anotherNOde
              loadOrCorrectNodeIDIntoHeap(otherNodeID)
            }
          case Unreachable => ;
          //we can reach an unreacable one in case two path from the removed centroid lead to the same node
        }
      }
    }

    markNodeUnreachableAndRemoveFromHeapIfPresent(removedCentroid)

    explore(removedCentroid)
  }

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  override def checkInternals(c: Checker){

    require(nodeIDHeap.isEmpty)

    val centroids:Iterable[Node] = this.centroids.value.toList.map(nodeID => graph.nodes(cbls.longToInt(nodeID)))
    val isConditionalEdgeOpen = (conditionID:Int) => this.openConditions.value contains conditionID

    //checking for each node the centroid (this is very costly: nbNodes*Dijkstra)
    for(node <- graph.nodes){

      val fromScratch = new DijkstraMT(this.graph).search(
        node,
        centroids,
        isConditionalEdgeOpen)

      val incremental = nodeLabeling(node.nodeId)
      require(fromScratch equals incremental, "node:" + node + " incremental:" + incremental + " fromScratch:" + fromScratch)
    }

    //this is mostly a static check
    for(node <- graph.nodes){
      trackedNodeToDistanceAndCentroidMap.get(node.nodeId) match{
        case None =>
          require(trackedNodeToDistanceAndCentroid(node.nodeId) == null)

        case Some((distanceVar,centroidVar)) =>
          require(trackedNodeToDistanceAndCentroid(node.nodeId).centroid == centroidVar)
          require(trackedNodeToDistanceAndCentroid(node.nodeId).distance == distanceVar)

          //this is the non-static stuff
          trackedNodeToDistanceAndCentroid(node.nodeId).checkEqual(nodeLabeling(node.nodeId))
      }
    }
  }
}

