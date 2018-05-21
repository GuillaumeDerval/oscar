package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.Objective
import oscar.cbls.core.search.{Neighborhood, _}

import scala.collection.immutable.{SortedMap, SortedSet}



class MoveExplorerAlgo(v:Int,
                       vehicleToRoutedNodes:SortedMap[Int,Iterable[Int]],
                       unroutedNodesToInsert:Iterable[Int],
                       nodeToRelevantVehicles:Map[Int,Iterable[Int]],

                       nodeVehicleToInsertNeighborhood:(Int,Int) => Neighborhood,
                       nodeTargetVehicleToMoveNeighborhood:(Int,Int) => Neighborhood,
                       nodeToRemoveNeighborhood:Int => Neighborhood,

                       removeAndReInsert:Int => () => Unit,

                       vehicleToObjectives:Array[Objective],
                       unroutedNodesPenalty:Objective,
                       globalObjective:Objective) {


  val initialVehicleToObjectives = vehicleToObjectives.map(_.value)
  val initialUnroutedNodesPenalty = unroutedNodesPenalty.value
  val initialGlobalObjective = globalObjective.value

  //TODO: find best loop nesting WRT. checkpoint calculation.
  //maybe we should unroute all nodes before doing move exploration since we do not want to waste time on evaluation obj on non targeted vehicle?
  val nodesToMove:Iterable[Int] = vehicleToRoutedNodes.flatMap(_._2)

  var nodes:Array[Node] = null
  var nodeIDToNode:SortedMap[Int,Node] = null
  var relevantVehicles:SortedSet[Int] = null
  var edgeBuilder:VLSNEdgeBuilder = null
  var trashNode:Node = null
  val vehicleToNode:Array[Node] = Array.fill(v)(null)

  var nbLabels:Int = -2

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
    edgeBuilder = new VLSNEdgeBuilder(nodes,nbLabels,v) //the labels are the vehicles
    exploreInsertions()
    exploreNodeMove()
    exploreDeletions() //should be called after insertions
    addNoMoveEdgesVehiclesToTrashNode()
    addTrashNodeToUnroutedNodes()

    edgeBuilder.finish()
  }

  private def buildNodes(){
    //label of nodes are:
    // for each routed node and vehicle node: the vehicle of the node
    // For each unrouted node: a diffeent label
    // a different label for the trashNode

    //as labels, we take the vehicles, plus one label per non-routed node
    val builder = new VLSNNodeBuilder(nbLabels = v)
    nodeIDToNode = SortedMap.empty

    //noeuds symboliques pour les véhicules
    relevantVehicles = SortedSet.empty ++ nodeToRelevantVehicles.flatMap(_._2)

    for(vehicle <- relevantVehicles){
      val node = builder.addNode(vehicle,vehicle,vehicle,VLSNSNodeType.VehicleNode)
      vehicleToNode(vehicle) = node
      nodeIDToNode += ((vehicle,node))
    }

    //noeud cible pour l'unroutage, label is v
    trashNode = builder.addNode(-1,-1,builder.newFreshLabel(),VLSNSNodeType.FictiveNode)

    //noeuds pour les noeud à déplacer
    for((vehicle,routedNodesOnVehicle) <- vehicleToRoutedNodes){
      require(vehicle < v)
      for(nodeID <- routedNodesOnVehicle){
        require(nodeID >= v, "cannot put vehicle to move :" + nodeID)
        nodeIDToNode += ((nodeID,builder.addNode(nodeID,vehicle,vehicle,VLSNSNodeType.RegularNode)))
      }
    }

    //noeuds non routés
    for(unroutedNode <- unroutedNodesToInsert){
      //TODO: check that label and vehicles are fine...
      nodeIDToNode += ((unroutedNode,builder.addNode(unroutedNode,v,builder.newFreshLabel(),VLSNSNodeType.UnroutedNode)))
    }

    val x = builder.finish()
    nodes = x._1
    nbLabels = x._2
  }


  private def explore(n:Neighborhood,localObj:Objective):Option[(Move,Int)] = {
    val initialObjective = localObj.value

    //we accept all moves, since degrading moves are allowed in negative cycles
    n.getMove(localObj,initialObjective,acceptanceCriterion = (_,newObj) => newObj != Int.MaxValue) match{
      case NoMoveFound => None
      case MoveFound(m) => Some((m.asInstanceOf[Move],m.objAfter - initialObjective))
    }
  }

  val maxInt = Int.MaxValue
  val acceptAllButMaxInt:(Int,Int)=>Boolean = (_,newObj:Int) => newObj != maxInt

  @inline
  def evaluateInsertOnVehicleNoRemove(unroutedNodeToInsert:Int,
                                      targetVehicleForInsertion:Int):(Move,Int) = {

    nodeVehicleToInsertNeighborhood(unroutedNodeToInsert, targetVehicleForInsertion).
      getMove(globalObjective,initialGlobalObjective,acceptanceCriterion = acceptAllButMaxInt) match {
      case NoMoveFound => null
      case MoveFound(move) =>
        val delta = move.objAfter - initialGlobalObjective
        (move,delta)
    }
  }

  @inline
  def evaluateInsertOnVehicleWithRemove(unroutedNodeToInsert:Int,
                                        targetVehicleForInsertion:Int,
                                        removedNode:Int,
                                        correctedGlobalInit:Int):(Move,Int) = {
    nodeVehicleToInsertNeighborhood(unroutedNodeToInsert, targetVehicleForInsertion).
      getMove(globalObjective,correctedGlobalInit,acceptanceCriterion = acceptAllButMaxInt) match {
      case NoMoveFound => null
      case MoveFound(move) =>
        val delta = move.objAfter - correctedGlobalInit
        (move,delta)
    }
  }

  private def exploreInsertions(){
    val vehicleAndUnroutedNodes:Iterable[(Int,Int)] =
      unroutedNodesToInsert.flatMap(unroutedNode => nodeToRelevantVehicles(unroutedNode).map(vehicle => (vehicle,unroutedNode)))

    val vehicleToUnroutedNodeToInsert = vehicleAndUnroutedNodes.groupBy(_._1).mapValues(_.map(_._2))

    for((targetVehicleForInsertion,unroutedNodesToInsert) <- vehicleToUnroutedNodeToInsert){

      var feasibleInsertForThisVehicle:Option[Move] = None
      //try inserts without removes
      for(unroutedNodeToInsert <- unroutedNodesToInsert) {
        //insertion without remove

        evaluateInsertOnVehicleNoRemove(
          unroutedNodeToInsert:Int,
          targetVehicleForInsertion:Int) match{
          case null => ;
          case (move,delta) =>
            val symbolicNodeToInsert = nodeIDToNode(unroutedNodeToInsert)
            edgeBuilder.addEdge(symbolicNodeToInsert, nodeIDToNode(targetVehicleForInsertion), delta, move, VLSNMoveType.InsertNoEject)
        }
      }

      //insertion with remove, we remove, and then insert
      //insertion with remove
      for(routingNodeToRemove <- vehicleToRoutedNodes(targetVehicleForInsertion)){
        val symbolicNodeToRemove = nodeIDToNode(routingNodeToRemove)

        //performing the remove
        val reInsert = removeAndReInsert(routingNodeToRemove)

        val unroutedObjAfterRemove = unroutedNodesPenalty.value
        val correctedGlobalInit = initialGlobalObjective - initialUnroutedNodesPenalty + unroutedObjAfterRemove

        for(unroutedNodeToInsert <- unroutedNodesToInsert) {
          //insertion without remove

          //Evaluating the delta
          evaluateInsertOnVehicleWithRemove(
            unroutedNodeToInsert:Int,
            targetVehicleForInsertion:Int,
            routingNodeToRemove:Int,
            correctedGlobalInit:Int) match{
            case null => ;
            case (move,delta) =>
              val symbolicNodeToInsert = nodeIDToNode(unroutedNodeToInsert)
              edgeBuilder.addEdge(symbolicNodeToInsert, symbolicNodeToRemove, delta, move, VLSNMoveType.InsertWithEject)
          }
        }
        //re-inserting
        reInsert()
      }
    }
  }

  def evaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle:Int, targetVehicleID: Int):(Move,Int) =
    myEvaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle:Int, targetVehicleID: Int):(Move,Int)

  def evaluateMoveToVehicleWithRemove(routingNodeToMove:Int, fromVehicle:Int, targetVehicleID:Int, removedNode:Int):(Move,Int) =
    myEvaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle:Int, targetVehicleID: Int)

  //we must put this in a separated function otherwise tehre is an issue with caching for the incremental VLSN
  def myEvaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle:Int, targetVehicleID: Int):(Move,Int) = {
    nodeTargetVehicleToMoveNeighborhood(routingNodeToMove, targetVehicleID)
      .getMove(vehicleToObjectives(targetVehicleID), initialVehicleToObjectives(targetVehicleID), acceptanceCriterion = acceptAllButMaxInt) match {
      case NoMoveFound => null
      case MoveFound(move) =>
        val delta = move.objAfter - initialVehicleToObjectives(targetVehicleID)
        (move,delta)
    }
  }

  private def exploreNodeMove(): Unit = {
    val vehicleAndNodeToMove:Iterable[(Int,Int)] =
      nodesToMove.flatMap(nodeToMove => nodeToRelevantVehicles(nodeToMove).map(vehicle => (vehicle,nodeToMove)))

    val vehicleToNodeToMoveThere = vehicleAndNodeToMove.groupBy(_._1).mapValues(_.map(_._2))

    for((targetVehicleID,routedNodesToMoveThere) <- vehicleToNodeToMoveThere) {
      val symbolicNodeOfVehicle = nodeIDToNode(targetVehicleID)

      //moves without removes
      for (routingNodeToMove <- routedNodesToMoveThere) {
        val symbolicNodeOfNodeToMove = nodeIDToNode(routingNodeToMove)
        if (symbolicNodeOfNodeToMove.vehicle != targetVehicleID){
          //move without remove
          //     :(Int,Int) => Neighborhood,
          evaluateMoveToVehicleNoRemove(routingNodeToMove:Int, symbolicNodeOfNodeToMove.vehicle,targetVehicleID:Int) match{
            case null => ;
            case (move,delta) =>
              edgeBuilder.addEdge(symbolicNodeOfNodeToMove, symbolicNodeOfVehicle, delta, move, VLSNMoveType.MoveNoEject)
          }
        }
      }
      //moves with removes
      for(nodeIDToEject <- vehicleToRoutedNodes(targetVehicleID)){
        val symbolicNodeToEject = nodeIDToNode(nodeIDToEject)

        //performing the remove
        val reInsert = removeAndReInsert(nodeIDToEject)

        val unroutedObjAfterRemove = unroutedNodesPenalty.value

        for(routingNodeToMove <- routedNodesToMoveThere) {
          val symbolicNodeOfNodeToMove = nodeIDToNode(routingNodeToMove)

          if (symbolicNodeOfNodeToMove.vehicle != targetVehicleID) {
            //Evaluating all moves on this remove
            evaluateMoveToVehicleWithRemove(routingNodeToMove, symbolicNodeOfNodeToMove.vehicle,targetVehicleID, nodeIDToEject) match{
              case null => ;
              case (move,delta) =>
                edgeBuilder.addEdge(symbolicNodeOfNodeToMove, symbolicNodeToEject, delta, move, VLSNMoveType.MoveWithEject)
            }
          }
        }
        //re-inserting
        reInsert()
      }
    }
  }


  def evaluateRemove(routingNodeToRemove:Int,fromVehicle:Int):(Move,Int) = {
    nodeToRemoveNeighborhood(routingNodeToRemove)
      .getMove(unroutedNodesPenalty,initialUnroutedNodesPenalty,acceptanceCriterion = (_,newObj) => newObj != Int.MaxValue) match{
      case NoMoveFound => null
      case MoveFound(move) =>
        val delta = move.objAfter - initialUnroutedNodesPenalty
        (move,delta)
    }

  }
  /**
    * deletions are from deleted node to trashNode
    */
  private def exploreDeletions(): Unit = {
    for ((vehicleID, routingNodesToRemove) <- vehicleToRoutedNodes) {
      for (routingNodeToRemove <- routingNodesToRemove) {
        evaluateRemove(routingNodeToRemove:Int,vehicleID) match{
          case null => ;
          case (move,delta) =>
            val symbolicNodeOfNodeToRemove = nodeIDToNode(routingNodeToRemove)
            edgeBuilder.addEdge(symbolicNodeOfNodeToRemove, trashNode, delta, move, VLSNMoveType.Remove)
        }
      }
    }
  }

  //should be called after all edges going to vehicle are generated
  private def addNoMoveEdgesVehiclesToTrashNode(): Unit ={
    for(vehicleNode <- vehicleToNode if vehicleNode != null){
      edgeBuilder.addEdge(vehicleNode,trashNode,0,null,VLSNMoveType.Symbolic)
    }
  }

  private def addTrashNodeToUnroutedNodes(): Unit ={
    for(unroutedNode <- unroutedNodesToInsert){
      edgeBuilder.addEdge(trashNode,nodeIDToNode(unroutedNode),0,null,VLSNMoveType.Symbolic)
    }
  }
}
