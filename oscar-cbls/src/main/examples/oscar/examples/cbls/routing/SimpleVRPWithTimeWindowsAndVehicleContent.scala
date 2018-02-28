package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.PDPConstraints
import oscar.examples.cbls.routing.SimpleVRPWithTimeWindow.chainsExtension

/**
  * Created by fg on 12/05/17.
  */

object SimpleVRPWithTimeWindowsAndVehicleContent extends App{
  val m = new Store(noCycle = false/*, checker = Some(new ErrorChecker)*/)
  val v = 10
  val n = 1000
  val penaltyForUnrouted = 10000
  val maxVehicleContent = 8
  val minVehicleContent = 4

  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val travelDurationMatrix = RoutingMatrixGenerator.generateLinearTravelTimeFunction(n,symmetricDistance)
  val (listOfChains,precedences) = RoutingMatrixGenerator.generateChainsPrecedence(n,v,(n-v)/2)
  val (earlylines, deadlines, taskDurations, maxWaitingDurations) = RoutingMatrixGenerator.generateFeasibleTimeWindows(n,v,travelDurationMatrix,listOfChains)
  val maxTravelDurations = RoutingMatrixGenerator.generateMaxTravelDurations(listOfChains,earlylines,travelDurationMatrix)
  val contentsFlow = RoutingMatrixGenerator.generateContentFlow(n,listOfChains,maxVehicleContent)
  val vehiclesSize = RoutingMatrixGenerator.generateVehiclesSize(v,maxVehicleContent,minVehicleContent)

  val myVRP =  new VRP(m,n,v)
  TimeWindowHelper.reduceTimeWindows(myVRP,travelDurationMatrix,maxTravelDurations,earlylines,deadlines,taskDurations)

  // Distance
  val totalRouteLength = constantRoutingDistance(myVRP.routes,n,v,false,symmetricDistance,true,true,false)(0)

  //TimeWindow
  val tiweWindowInvariant = forwardCumulativeIntegerIntegerDimensionOnVehicle(
    myVRP.routes,n,v,
    (fromNode,toNode,arrivalTimeAtFromNode,leaveTimeAtFromNode)=> {
      val arrivalTimeAtToNode = leaveTimeAtFromNode + travelDurationMatrix.getTravelDuration(fromNode,0,toNode)
      val leaveTimeAtToNode =
        if(toNode < v) 0
        else Math.max(arrivalTimeAtToNode,earlylines(toNode)) + taskDurations(toNode)
      (arrivalTimeAtToNode,leaveTimeAtToNode)
    },
    Array.tabulate(v)(x => new CBLSIntConst(0)),
    Array.tabulate(v)(x => new CBLSIntConst(earlylines(x)+taskDurations(x))),
    0,
    0,
    contentName = "Time at node"
  )
  val timeWindowExtension = timeWindow(earlylines,deadlines,taskDurations,maxWaitingDurations)

  // Vehicle content
  val violationOfContentAtNode = new CBLSIntVar(myVRP.routes.model, 0, 0 to Int.MaxValue, "violation of capacity " + "Content at node")
  val capacityInvariant = forwardCumulativeConstraintOnVehicle(myVRP.routes,n,v,
    (from,to,fromContent) => fromContent + contentsFlow(to),
    maxVehicleContent,
    vehiclesSize.map(maxVehicleContent-_),
    violationOfContentAtNode,
    4,
    "Content at node")

  //Chains
  val precedenceInvariant = precedence(myVRP.routes,precedences)
  val chainsExtension = chains(myVRP,listOfChains)

  //Constraints & objective
  val (fastConstrains,slowConstraints) = PDPConstraints(myVRP,
    timeWindow = Some(timeWindowExtension),
    timeWindowInvariant = Some(tiweWindowInvariant),
    capacityInvariant = Some(capacityInvariant),
    precedences = Some(precedenceInvariant),
    maxTravelDurations = Some(maxTravelDurations))
  val obj = new CascadingObjective(fastConstrains,
    new CascadingObjective(slowConstraints,
      totalRouteLength + (penaltyForUnrouted*(n - length(myVRP.routes)))))

  m.close()

  val relevantToTime = TimeWindowHelper.relevantPredecessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)
  val relevantToCapacity = CapacityHelper.relevantPredecessorsOfNodes(myVRP, maxVehicleContent, vehiclesSize, contentsFlow)

  val relevantPredecessorsOfNodes = relevantToTime.map(x => x._1 -> x._2.filter(relevantToCapacity(x._1)))
  val relevantSuccessorsOfNodes = TimeWindowHelper.relevantSuccessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance,relevantPredecessorsOfNodes))

  def relevantPredecessorsForLastNode(lastNode: Int) = ChainsHelper.relevantNeighborsForLastNodeAfterHead(myVRP,chainsExtension,Some(relevantPredecessorsOfNodes(lastNode)))(lastNode)
  val relevantPredecessorsForInternalNodes = ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP, chainsExtension)_

  var enoughSpaceAfterNeighborNow: (Int,Int,Array[Int]) => Boolean =
    CapacityHelper.enoughSpaceAfterNeighbor(n,capacityInvariant)

  def postFilter(node:Int): (Int) => Boolean = {
    val routedNode = myVRP.routed.value
    (neighbor: Int) => {
      val successor = myVRP.nextNodeOf(neighbor)
      routedNode.contains(neighbor) &&
      (successor.isEmpty || relevantSuccessorsOfNodes(node).contains(successor.get)) &&
        enoughSpaceAfterNeighborNow(node,neighbor,contentsFlow)
    }
  }


  // MOVING


  val nextMoveGenerator = {
    (exploredMoves:List[OnePointMoveMove], t:Option[List[Int]]) => {
      val chainTail: List[Int] = t match {
        case None =>
          val movedNode = exploredMoves.head.movedPoint
          chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(movedNode))
        case Some(tail: List[Int]) => tail
      }

      chainTail match {
        case Nil => None
        case head :: Nil => None
        case nextNodeToMove :: newTail =>
          val moveNeighborhood = onePointMove(() => Some(nextNodeToMove),
            () => relevantPredecessorsForInternalNodes, myVRP)
          Some(moveNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainMove = onePointMove(() => myVRP.routed.value.filter(chainsExtension.isHead),()=> myVRP.kFirst(v*2,closestRelevantNeighborsByDistance,postFilter), myVRP,neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode:Int) = onePointMove(() => List(lastNode),()=> myVRP.kFirst(v*2,relevantPredecessorsForLastNode,postFilter), myVRP,neighborhoodName = "MoveLastOfChain")

  val oneChainMove = {
    dynAndThen(firstNodeOfChainMove,
      (moveMove: OnePointMoveMove) => {
        mu[OnePointMoveMove, Option[List[Int]]](
          lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
          nextMoveGenerator,
          None,
          Int.MaxValue,
          false)
      }
    ) name "One Chain Move"

  }

  def onePtMove(k:Int) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,closestRelevantNeighborsByDistance,postFilter), myVRP)) name "One Point Move"

  // INSERTING

  val nextInsertGenerator = {
    (exploredMoves:List[InsertPointMove], t:Option[List[Int]]) => {
      val chainTail: List[Int] = t match {
        case None =>
          val insertedNode = exploredMoves.head.insertedPoint
          chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(insertedNode))
        case Some(tail: List[Int]) => tail
      }

      chainTail match {
        case Nil => None
        case head :: Nil => None
        case nextNodeToInsert :: newTail =>
          val insertNeighborhood = insertPointUnroutedFirst(() => Some(nextNodeToInsert),
            () => relevantPredecessorsForInternalNodes, myVRP)
          Some(insertNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainInsertion = insertPointUnroutedFirst(() => chainsExtension.heads.filter(n => !myVRP.isRouted(n)),()=> myVRP.kFirst(v*2,closestRelevantNeighborsByDistance, postFilter), myVRP,neighborhoodName = "InsertUF")

  def lastNodeOfChainInsertion(lastNode:Int) = insertPointUnroutedFirst(() => List(lastNode),()=> myVRP.kFirst(v*2,relevantPredecessorsForLastNode,postFilter), myVRP,neighborhoodName = "InsertUF")

  val oneChainInsert = {
    dynAndThen(firstNodeOfChainInsertion,
      (insertMove: InsertPointMove) => {
        mu[InsertPointMove,Option[List[Int]]](
          lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
          nextInsertGenerator,
          None,
          Int.MaxValue,
          false)
      }) name "One Chain Insert"

  }

  //val routeUnroutedPoint =  Profile(new InsertPointUnroutedFirst(myVRP.unrouted,()=> myVRP.kFirst(10,filteredClosestRelevantNeighborsByDistance), myVRP,neighborhoodName = "InsertUF"))


  val search = bestSlopeFirst(List(oneChainInsert,oneChainMove,onePtMove(20)))afterMove(
    enoughSpaceAfterNeighborNow = CapacityHelper.enoughSpaceAfterNeighbor(n,capacityInvariant))
  //val search = (BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, vlsn1pt)))


  search.verbose = 1
  //search.verboseWithExtraInfo(4, ()=> "" + myVRP)



  search.doAllMoves(obj=obj)

  println(myVRP)

  search.profilingStatistics
}
