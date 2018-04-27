package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.core.search.{DoNothingMove, Move}

import scala.collection.immutable.{SortedMap, SortedSet}


class MoveExplorerAlgo(v:Int,
                       vehicleToRoutedNodes:SortedMap[Int,Iterable[Int]],
                       unroutedNodesToInsert:Iterable[Int],
                       insertNodeOnVehicleToMoveAndGain:(Int,Int,Option[Int]) => Option[(Move,Int)],
                       moveNodeToVehicleToMoveAndGain:(Int,Int) => Option[(Move,Int)],
                       removeNodeToMoveAndGain:(Int => Option[(Move,Int)]),
                       removeAndReInsert:Int => () => Unit,
                       nodeToRelevantVehicles:Int => Iterable[Int],
                       initialObj:Int) {

  var nodes:Array[Node] = null
  var nodeIDToNode:SortedMap[Int,Node] = null
  var relevantVehicles:SortedSet[Int] = null
  var edgeBuilder:VLSNEdgeBuilder = null
  var vehicleWithSomeIncomingEdges:SortedSet[Int] = SortedSet.empty
  var nonRoutedNodesWithOneInsertion:List[Int] = List.empty
  var trashNode:Node = null

  def buildGraph():VLSNGraph = {

    //nodes are all the nodes to consider times all the moe that moves them (so we have a voca

    //nodes of the moveGraph are:
    // if the point is routed: removing a node from the vehicle where it is
    // if the point is not routed: routing it if not in the sequence
    //there is a trashNode representing non-routed nodes. only for symbolic purpose.
    //edges are:
    // if from and to are routed: the move that moves the from point to the vehicle of the to point, assuming the to point has been removed from its vehicle
    // if from is routed, and to is not routed: removing the from, assuming the to has been inserted (these one are generated automatically, by just removing points one after the other and evaluating the penalty for unrouting)
    // if the from is not routed and the to is routed: routing the from on the vehicle of the to, assuming the to has been removed from its vehicle

    //there is an edge from trashNode to all node (routed or unrouted), with NoMove and delta 0 ??

    //it allows simple insertion without delete, in teh case of non-routed nodes,


    //et il faut aussi de edges pour représenter l'insertion/move sans déplacement d'autre noeuds.
    //donc on ajoute des noeud qui représentent les véhicules pour insertion, et un edge qui pointe vers un tel noeud représente un moveinsertion vers ce v"hcule sans déplacement de noeud de ce véhicule.
    //pour tenir comte de çà dans la détection de cycles, on ajouter de sedges à cût zéo et sans mouvement de tous es véhicules vers tous les edges.

    //et pour les delete sans insertion?

    buildNodes()
    edgeBuilder = new VLSNEdgeBuilder(nodes)
    exploreInsertions(initialObj)
    exploreNodeMove()
    exploreDeletions() //should be called after insertions
    addNoMoveEdgesForVehicles() //should be the last one called

    edgeBuilder.finish()
  }

  def buildNodes(){
    val builder = new VLSNNodeBuilder()
    nodeIDToNode = SortedMap.empty

    //noeud cible pour l'unroutage
    trashNode = builder.addNode(-1,-1)

    //noeuds symboliques pour les véhicules
    relevantVehicles = SortedSet.empty ++ nodeToRelevantVehicles.flatMap(_._2)

    for(vehicle <- relevantVehicles){
      val nodeObj = builder.addNode(vehicle,vehicle)
      nodeIDToNode += ((vehicle ,nodeObj))
    }

    //noeuds pour les noeud à déplacer
    for((vehicle,routedNodesOnVehicle) <- vehicleToRoutedNodes){
      for(nodeID <- routedNodesOnVehicle){
        val nodeObj = builder.addNode(nodeID,vehicle)
        nodeIDToNode += ((nodeID,nodeObj))
      }
    }
  }

  def exploreInsertions(initialObj:Int){

    for(routingNodeToInsert <- unroutedNodesToInsert){
      var anyPossibleInsert = false
      val symbolicNodeToInsert = nodeIDToNode(routingNodeToInsert)
      for(targetVehicleForInsertion <- nodeToRelevantVehicles(routingNodeToInsert)){

        //insertion without remove
        insertNodeOnVehicleToMoveAndGain(routingNodeToInsert,targetVehicleForInsertion,Some(initialObj)) match{
          case None =>
          case Some((move,gain)) =>
            val symbolicNodeOfVehicle = nodeIDToNode(targetVehicleForInsertion)
            edgeBuilder.addEdge(symbolicNodeToInsert,symbolicNodeOfVehicle,gain,move)
            vehicleWithSomeIncomingEdges += targetVehicleForInsertion
            anyPossibleInsert = true
        }

        //insertion with remove
        for(routingNodeToRemove <- vehicleToRoutedNodes(targetVehicleForInsertion)){
          val symbolicNodeToRemove = nodeIDToNode(routingNodeToRemove)

          //performing the remove
          val reInsert = removeAndReInsert(routingNodeToRemove)

          //Evaluating the delta
          insertNodeOnVehicleToMoveAndGain(routingNodeToInsert,targetVehicleForInsertion) match{
            case None =>
            case Some((move,gain)) =>
              edgeBuilder.addEdge(symbolicNodeToInsert,symbolicNodeToRemove,gain,move)
              anyPossibleInsert = true
          }

          //re-inserting
          reInsert()
        }
      }

      if(anyPossibleInsert){
        nonRoutedNodesWithOneInsertion = routingNodeToInsert ::  nonRoutedNodesWithOneInsertion
      }
    }
  }

  def exploreNodeMove(): Unit ={
    for((vehicleID,routingNodesToMove) <- vehicleToRoutedNodes){
      for(routingNodeToMove <- routingNodesToMove){
        val symbolicNodeOfNodeToMove = nodeIDToNode(routingNodeToMove)

        for(targetVehicleID <- nodeToRelevantVehicles(routingNodeToMove)){

          //move without remove
          moveNodeToVehicleToMoveAndGain(routingNodeToMove,targetVehicleID) match{
            case None =>
            case Some((move,gain)) =>
              val symbolicNodeOfVehicle = nodeIDToNode(targetVehicleID)
              edgeBuilder.addEdge(symbolicNodeOfNodeToMove,symbolicNodeOfVehicle,gain,move)
              vehicleWithSomeIncomingEdges += targetVehicleID
          }

          //move with remove
          //TODO: how about inverting the order of loops and removing a node, then tring all moves to the target vehicle, so we perform the remove only once.
          for(nodeIDToEject <- vehicleToRoutedNodes(targetVehicleID)){
            val symbolicNodeToEject = nodeIDToNode(nodeIDToEject)

            //performing the remove
            val reInsert = removeAndReInsert(nodeIDToEject)

            //Evaluating the delta
            moveNodeToVehicleToMoveAndGain(routingNodeToMove,nodeIDToEject) match{
              case None =>
              case Some((move,gain)) =>
                edgeBuilder.addEdge(symbolicNodeOfNodeToMove,symbolicNodeToEject,gain,move)
                vehicleWithSomeIncomingEdges += targetVehicleID
            }

            //re-inserting
            reInsert()
          }
        }
      }
    }
  }

  /**
    * deletions are from deleted node to trashNode
    */
  def exploreDeletions(): Unit = {
    if (nonRoutedNodesWithOneInsertion.isEmpty) return

    val symbolicNodesOfNonRoutedNodesWithPossibleInsert =
      nonRoutedNodesWithOneInsertion.map(nodeIDToNode)

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
  def addNoMoveEdgesForVehicles(): Unit ={
    for(vehicleID <- vehicleWithSomeIncomingEdges){
      val symblicNodeOFVehicle = nodeIDToNode(vehicleID)
      for(symbolicNode <- nodes){
        if(symbolicNode.representedNode < v){
          edgeBuilder.addEdge(symbolicNode,symblicNodeOFVehicle,0,new DoNothingMove(0))
        }
      }
    }
  }
}

