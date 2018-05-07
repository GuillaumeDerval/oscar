package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.core.search.{DoNothingMove, Move}

import scala.collection.immutable.{SortedMap, SortedSet}



class MoveExplorerAlgo(v:Int,
                       vehicleToRoutedNodes:SortedMap[Int,Iterable[Int]],
                       unroutedNodesToInsert:Iterable[Int],
                       nodeToRelevantVehicles:Map[Int,Iterable[Int]],

                       insertNodeOnVehicleToMoveAndGain:(Int,Int,Option[Int]) => Option[(ComposableMove,Int)],
                       moveNodeToVehicleToMoveAndGain:(Int,Int) => Option[(ComposableMove,Int)],
                       removeNodeToMoveAndGain:(Int => Option[(ComposableMove,Int)]),
                       removeAndReInsert:Int => () => Unit,

                       initialObj:Int) {

  val nodesToMove:Iterable[Int] = vehicleToRoutedNodes.flatMap(_._2)

  var nodes:Array[Node] = null
  var nodeIDToNode:SortedMap[Int,Node] = null
  var relevantVehicles:SortedSet[Int] = null
  var edgeBuilder:VLSNEdgeBuilder = null
  var trashNode:Node = null
  val vehicleToNode:Array[Node] = Array.fill(v)(null)

  def buildGraph():VLSNGraph = {

    //nodes are all the nodes to consider, ll the vehicles, and a trashNode

    //nodes of the moveGraph are:
    // if the point is routed: removing a node from the vehicle where it is
    // if the point is not routed: routing it if not in the sequence
    //there is a trashNode representing non-routed nodes. only for symbolic purpose.
    //edges are:
    // if from and to are routed: the move that moves the from point to the vehicle of the to point, assuming the to point has been removed from its vehicle
    // if from is routed, and to is a vehicle: moving a node to the vehicle without removing othernode from the vehicle
    // if from is not routed, and to is a vehicle: inserting th node onto the vehicle without removing other node from the vehicle (althoug hthis move is very simple and should not be part of VLSN explration...)
    // if from is routed, and to is not routed: removing the from, assuming the to has been inserted (these one are generated automatically, by just removing points one after the other and evaluating the penalty for unrouting)
    // if the from is not routed and the to is routed: routing the from on the vehicle of the to, assuming the to has been removed from its vehicle
    //there is a noMove edge from each vehicle to TrashNode, and a noMove edge fro trashNode to all unrouted node and all routed nodes

    //label of nodes are:
    // for each routed node and vehicle node: the vehicle of the node
    // For each uinrouted node: a diffeent label
    // a different label for the trashNode

    buildNodes()
    edgeBuilder = new VLSNEdgeBuilder(nodes,v) //the labels are the vehicles
    exploreInsertions(initialObj)
    exploreNodeMove()
    exploreDeletions() //should be called after insertions
    addNoMoveEdgesVehiclesToTrashNode()
    addTrashNodeToUnroutedNodes()
    addTrashNodeToRoutedNodes()

    edgeBuilder.finish()
  }

  def buildNodes(){
    //label of nodes are:
    // for each routed node and vehicle node: the vehicle of the node
    // For each uinrouted node: a diffeent label
    // a different label for the trashNode

    //as labels, we take the vehicles, plus one label per non-routed node
    val builder = new VLSNNodeBuilder(nbLabels = v + unroutedNodesToInsert.size + 1)
    nodeIDToNode = SortedMap.empty

    //noeuds pour les noeud à déplacer
    for((vehicle,routedNodesOnVehicle) <- vehicleToRoutedNodes){
      for(nodeID <- routedNodesOnVehicle){
        nodeIDToNode += ((nodeID,builder.addNode(nodeID,vehicle,vehicle)))
      }
    }

    //noeud cible pour l'unroutage, label is v
    trashNode = builder.addNode(-1,-1,v)

    //noeuds symboliques pour les véhicules
    relevantVehicles = SortedSet.empty ++ nodeToRelevantVehicles.flatMap(_._2)

    for(vehicle <- relevantVehicles){
      val node = builder.addNode(vehicle,vehicle,vehicle)
      vehicleToNode(vehicle) = node
      nodeIDToNode += ((vehicle,node))
    }
  }

  def exploreInsertions(initialObj:Int){
    val vehicleAndUnroutedNodes:Iterable[(Int,Int)] =
      unroutedNodesToInsert.flatMap(unroutedNode => nodeToRelevantVehicles(unroutedNode).map(vehicle => (vehicle,unroutedNode)))

    val vehicleToUnroutedNodeToInsert = vehicleAndUnroutedNodes.groupBy(_._1).mapValues(_.map(_._2))

    for((targetVehicleForInsertion,unroutedNodesToInsert) <- vehicleToUnroutedNodeToInsert){

      //try inserts without removes
      for(unroutedNodeToInsert <- unroutedNodesToInsert) {
        //insertion without remove
        val symbolicNodeToInsert = nodeIDToNode(unroutedNodeToInsert)
        insertNodeOnVehicleToMoveAndGain(unroutedNodeToInsert, targetVehicleForInsertion, Some(initialObj)) match {
          case None =>
          case Some((move, gain)) =>
            edgeBuilder.addEdge(symbolicNodeToInsert, nodeIDToNode(targetVehicleForInsertion), gain, move)
        }
      }

      //insertion with remove, we remove, and then insert
      //insertion with remove
      for(routingNodeToRemove <- vehicleToRoutedNodes(targetVehicleForInsertion)){
        val symbolicNodeToRemove = nodeIDToNode(routingNodeToRemove)

        //performing the remove
        val reInsert = removeAndReInsert(routingNodeToRemove)

        for(unroutedNodeToInsert <- unroutedNodesToInsert) {
          //insertion without remove
          val symbolicNodeToInsert = nodeIDToNode(unroutedNodeToInsert)

          //Evaluating the delta
          insertNodeOnVehicleToMoveAndGain(unroutedNodeToInsert, targetVehicleForInsertion) match {
            case None =>
            case Some((move, gain)) =>
              edgeBuilder.addEdge(symbolicNodeToInsert, symbolicNodeToRemove, gain, move)
          }
        }
        //re-inserting
        reInsert()
      }
    }
  }

  def exploreNodeMove(): Unit ={
    val vehicleAndNodeToMove:Iterable[(Int,Int)] =
      nodesToMove.flatMap(nodeToMove => nodeToRelevantVehicles(nodeToMove).map(vehicle => (vehicle,nodeToMove)))

    val vehicleToNodeToMoveThere = vehicleAndNodeToMove.groupBy(_._1).mapValues(_.map(_._2))

    for((targetVehicleID,routedNodesToMoveThere) <- vehicleToNodeToMoveThere){

      //moves without removes
      for(routingNodeToMove <- routedNodesToMoveThere) {
        val symbolicNodeOfNodeToMove = nodeIDToNode(routingNodeToMove)

        //move without remove
        moveNodeToVehicleToMoveAndGain(routingNodeToMove, targetVehicleID) match {
          case None =>
          case Some((move, gain)) =>
            val symbolicNodeOfVehicle = nodeIDToNode(targetVehicleID)
            edgeBuilder.addEdge(symbolicNodeOfNodeToMove, symbolicNodeOfVehicle, gain, move)
        }
      }

      //moves with removes
      for(nodeIDToEject <- vehicleToRoutedNodes(targetVehicleID)){
        val symbolicNodeToEject = nodeIDToNode(nodeIDToEject)

        //performing the remove
        val reInsert = removeAndReInsert(nodeIDToEject)

        for(routingNodeToMove <- routedNodesToMoveThere) {
          val symbolicNodeOfNodeToMove = nodeIDToNode(routingNodeToMove)

          //Evaluating all moves on this remove
          moveNodeToVehicleToMoveAndGain(routingNodeToMove, nodeIDToEject) match {
            case None =>
            case Some((move, gain)) =>
              edgeBuilder.addEdge(symbolicNodeOfNodeToMove, symbolicNodeToEject, gain, move)
          }
        }
        //re-inserting
        reInsert()
      }
    }
  }

  /**
    * deletions are from deleted node to trashNode
    */
  def exploreDeletions(): Unit = {

    for ((vehicleID, routingNodesToRemove) <- vehicleToRoutedNodes) {
      for (routingNodeToRemove <- routingNodesToRemove) {

        removeNodeToMoveAndGain(routingNodeToRemove) match{
          case None => ;
          case Some((move,gain)) =>
            val symbolicNodeOfNodeToRemove = nodeIDToNode(routingNodeToRemove)
            edgeBuilder.addEdge(symbolicNodeOfNodeToRemove, trashNode, gain, move)
        }
      }
    }
  }

  //should be called after all edges going to vehicle are generated
  def addNoMoveEdgesVehiclesToTrashNode(): Unit ={
    for(vehicleNode <- vehicleToNode if vehicleNode != null){
      edgeBuilder.addEdge(vehicleNode,trashNode,0,new DoNothingMove(0))
    }
  }

  def addTrashNodeToUnroutedNodes(): Unit ={
    for(unroutedNode <- unroutedNodesToInsert){
      edgeBuilder.addEdge(trashNode,nodeIDToNode(unroutedNode),0,new DoNothingMove(0))
    }
  }

  def addTrashNodeToRoutedNodes(): Unit = {
    for(nodeToMove <- nodesToMove){
      edgeBuilder.addEdge(trashNode,nodeIDToNode(nodeToMove),0,new DoNothingMove(0))
    }
  }
}
