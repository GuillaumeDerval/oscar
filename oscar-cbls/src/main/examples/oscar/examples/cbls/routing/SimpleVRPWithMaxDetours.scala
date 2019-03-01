package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.business.routing._

/**
  * Created by fg on 12/05/17.
  */
object SimpleVRPWithMaxDetours extends App{
  val m = new Store(noCycle = false/*, checker = Some(new ErrorChecker)*/)
  val v = 10
  val n = 1000
  val penaltyForUnrouted = 10000
  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val travelDurationMatrix = RoutingMatrixGenerator.generateLinearTravelTimeFunction(n,symmetricDistance)
  val (listOfChains,precedences)= RoutingMatrixGenerator.generateChainsPrecedence(n,v,(n-v)/2)
  val (earliestArrivalTimes, latestLeavingTimes, taskDurations, maxWaitingDurations) = RoutingMatrixGenerator.generateFeasibleTimeWindows(n,v,travelDurationMatrix,listOfChains)
  val maxTravelDurations = RoutingMatrixGenerator.generateMaxTravelDurations(listOfChains,earliestArrivalTimes,travelDurationMatrix)
  val myVRP =  new VRP(m,n,v)
  TimeWindowHelper.reduceTimeWindows(myVRP,travelDurationMatrix,maxTravelDurations,earliestArrivalTimes,latestLeavingTimes,taskDurations)

  // Distance
  val totalRouteLength = routeLength(myVRP.routes,n,v,false,symmetricDistance,true,true,false)(0)

  //Chains
  val precedenceRoute = myVRP.routes.createClone()
  val precedenceInvariant = precedence(precedenceRoute,precedences)
  val vehicleOfNodesNow = vehicleOfNodes(precedenceRoute,v)
  val precedencesConstraints = new ConstraintSystem(m)
  for(start <- precedenceInvariant.nodesStartingAPrecedence)
    precedencesConstraints.add(vehicleOfNodesNow(start) === vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head))
  precedencesConstraints.add(0 === precedenceInvariant)
  val chainsExtension = chains(myVRP,listOfChains)

  //TimeWindow
  val timeWindowRoute = precedenceRoute.createClone()
  val timeWindowExtension = timeWindows(Some(earliestArrivalTimes), None, None, Some(latestLeavingTimes), taskDurations, None)
  val timeWindowConstraints = new ConstraintSystem(m)
  val timeWindowInvariant = forwardCumulativeIntegerIntegerDimensionOnVehicle(
    myVRP.routes,n,v,
    (fromNode,toNode,arrivalTimeAtFromNode,leaveTimeAtFromNode)=> {
      val arrivalTimeAtToNode = leaveTimeAtFromNode + travelDurationMatrix.getTravelDuration(fromNode,0,toNode)
      val leaveTimeAtToNode =
        if(toNode < v) 0
        else Math.max(arrivalTimeAtToNode,earliestArrivalTimes(toNode)) + taskDurations(toNode)
      (arrivalTimeAtToNode,leaveTimeAtToNode)
    },
    Array.tabulate(v)(x => new CBLSIntConst(0)),
    Array.tabulate(v)(x => new CBLSIntConst(earliestArrivalTimes(x)+taskDurations(x))),
    0,
    0,
    contentName = "Time at node"
  )
  val arrivalTimes = timeWindowInvariant.content1AtNode
  val leaveTimes = timeWindowInvariant.content2AtNode
  // TravelDuration constraint
  for(maxDetour <- maxTravelDurations){
    timeWindowConstraints.post(arrivalTimes(maxDetour._1.last) - leaveTimes(maxDetour._1.head) le maxDetour._2)
  }

  //Time window constraints
  val arrivalTimesAtEnd = timeWindowInvariant.content1AtEnd
  for(i <- 0 until n){
    if(i < v && latestLeavingTimes(i) != Long.MaxValue) {
      timeWindowConstraints.post((arrivalTimesAtEnd(i) le latestLeavingTimes(i)).nameConstraint("end of time for vehicle " + i))
    } else {
      if(latestLeavingTimes(i) != Long.MaxValue)
        timeWindowConstraints.post((leaveTimes(i) le latestLeavingTimes(i)).nameConstraint("end of time window on node " + i))
      if(maxWaitingDurations(i) != Long.MaxValue)
        timeWindowConstraints.post((arrivalTimes(i) ge earliestArrivalTimes(i)).nameConstraint("start of time window on node (with duration)" + i))
    }
  }


  //Objective function
  val obj = new CascadingObjective(precedencesConstraints,
    new CascadingObjective(timeWindowConstraints,
      totalRouteLength + (penaltyForUnrouted*(n - length(myVRP.routes)))))

  m.close()

  val relevantPredecessorsOfNodes = TimeWindowHelper.relevantPredecessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)
  val relevantSuccessorsOfNodes = TimeWindowHelper.relevantSuccessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance,relevantPredecessorsOfNodes)(_))

  def relevantPredecessorsForLastNode(lastNode: Long) = ChainsHelper.relevantNeighborsForLastNodeAfterHead(myVRP,chainsExtension,Some(relevantPredecessorsOfNodes(lastNode)))(lastNode)
  val relevantPredecessorsForInternalNodes = ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP, chainsExtension)_

  def postFilter(node:Long): (Long) => Boolean = {
    val routedNode = myVRP.routed.value
    (neighbor: Long) => {
      val successor = myVRP.nextNodeOf(neighbor)
      routedNode.contains(neighbor) &&
        (successor.isEmpty || relevantSuccessorsOfNodes(node).contains(successor.get))
    }
  }


  // MOVING
  val nextMoveGenerator = {
    (exploredMoves:List[OnePointMoveMove], t:Option[List[Long]]) => {
      val chainTail: List[Long] = t match {
        case None =>
          val movedNode = exploredMoves.head.movedPoint
          chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(movedNode))
        case Some(tail: List[Long]) => tail
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

  val firstNodeOfChainMove = onePointMove(() => myVRP.routed.value.filter(chainsExtension.isHead),()=> myVRP.kFirst(v*2,closestRelevantNeighborsByDistance(_),postFilter), myVRP,neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode:Long) = onePointMove(() => List(lastNode),()=> myVRP.kFirst(v*2,relevantPredecessorsForLastNode,postFilter), myVRP,neighborhoodName = "MoveLastOfChain")

  val oneChainMove = {
    dynAndThen(firstNodeOfChainMove,
      (moveMove: OnePointMoveMove) => {
        mu[OnePointMoveMove, Option[List[Long]]](
          lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
          nextMoveGenerator,
          None,
          Long.MaxValue,
          false)
      }
    ) name "One Chain Move"

  }

  def onePtMove(k:Long) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,closestRelevantNeighborsByDistance(_),postFilter), myVRP)) name "One Point Move"


  // INSERTING
  val nextInsertGenerator = {
    (exploredMoves:List[InsertPointMove], t:Option[List[Long]]) => {
      val chainTail: List[Long] = t match {
        case None =>
          val insertedNode = exploredMoves.head.insertedPoint
          chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(insertedNode))
        case Some(tail: List[Long]) => tail
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

  val firstNodeOfChainInsertion = insertPointUnroutedFirst(() => chainsExtension.heads.filter(n => !myVRP.isRouted(n)),()=> myVRP.kFirst(v*2,closestRelevantNeighborsByDistance(_), postFilter), myVRP,neighborhoodName = "InsertUF")

  def lastNodeOfChainInsertion(lastNode:Long) = insertPointUnroutedFirst(() => List(lastNode),()=> myVRP.kFirst(v*2,relevantPredecessorsForLastNode,postFilter), myVRP,neighborhoodName = "InsertUF")

  val oneChainInsert = {
    dynAndThen(firstNodeOfChainInsertion,
      (insertMove: InsertPointMove) => {
        mu[InsertPointMove,Option[List[Long]]](
          lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
          nextInsertGenerator,
          None,
          Long.MaxValue,
          false)
      }) name "One Chain Insert"

  }

  val search = bestSlopeFirst(List(oneChainInsert,oneChainMove,onePtMove(20)))

  search.verbose = 1

  search.doAllMoves(obj=obj)

  println(myVRP)

  search.profilingStatistics
}