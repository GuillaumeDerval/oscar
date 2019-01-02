package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.lib.invariant.seq.Precedence
import oscar.cbls.business.routing.invariants.{PDPConstraints, TimeWindowConstraint}
import oscar.cbls.core.search.Best
import oscar.cbls.lib.constraint.EQ

/**
  * Created by fg on 12/05/17.
  */

object SimpleVRPWithTimeWindow extends App{
  val m = new Store(noCycle = false)
  val v = 10
  val n = 1000
  val penaltyForUnrouted = 10000
  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val travelDurationMatrix = RoutingMatrixGenerator.generateLinearTravelTimeFunction(n,symmetricDistance)
  val (listOfChains,precedences) = RoutingMatrixGenerator.generateChainsPrecedence(n,v,(n-v)/2)
  val (earlylines, deadlines, taskDurations, maxWaitingDurations) = RoutingMatrixGenerator.generateFeasibleTimeWindows(n,v,travelDurationMatrix,listOfChains)

  val myVRP =  new VRP(m,n,v)

  // Distance
  val totalRouteLength = constantRoutingDistance(myVRP.routes,n,v,false,symmetricDistance,true,true,false)(0)

  //Chains
  val precedenceRoute = myVRP.routes.createClone()
  val precedenceInvariant = precedence(precedenceRoute,precedences)
  val vehicleOfNodesNow = vehicleOfNodes(precedenceRoute,v)
  val precedencesConstraints = new ConstraintSystem(m)
  for(start <- precedenceInvariant.nodesStartingAPrecedence)
    precedencesConstraints.add(EQ(vehicleOfNodesNow(start),vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head)))
  precedencesConstraints.add(EQ(0,precedenceInvariant))
  val chainsExtension = chains(myVRP,listOfChains)

  //TimeWindow
  val timeWindowRoute = precedenceRoute.createClone()
  val timeWindowExtension = timeWindow(earlylines, deadlines, taskDurations, maxWaitingDurations)
  val timeWindowViolations = Array.fill(v)(new CBLSIntVar(m, 0, Domain.coupleToDomain((0,1))))
  val timeMatrix = Array.tabulate(n)(from => Array.tabulate(n)(to => travelDurationMatrix.getTravelDuration(from, 0, to)))
  val smartTimeWindowInvariant =
    TimeWindowConstraint(myVRP.routes, n, v,
      earlylines,
      deadlines,
      taskDurations,
      timeMatrix, timeWindowViolations)

  //Objective function
  val obj = new CascadingObjective(precedencesConstraints,
    new CascadingObjective(sum(timeWindowViolations),
      totalRouteLength + (penaltyForUnrouted*(n - length(myVRP.routes)))))

  m.close()

  val relevantPredecessorsOfNodes = TimeWindowHelper.relevantPredecessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)
  val relevantSuccessorsOfNodes = TimeWindowHelper.relevantSuccessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)

  def postFilter(node:Int): (Int) => Boolean = {
    (neighbor: Int) => {
      val successor = myVRP.nextNodeOf(neighbor)
      myVRP.isRouted(neighbor) &&
        (successor.isEmpty || relevantSuccessorsOfNodes(node).contains(successor.get))
    }
  }

  val closestRelevantPredecessorsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance,relevantPredecessorsOfNodes))

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
            () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP,chainsExtension), myVRP)
          Some(moveNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainMove = onePointMove(
    () => myVRP.routed.value.filter(chainsExtension.isHead),
    ()=> myVRP.kFirst(v*2,closestRelevantPredecessorsByDistance,postFilter), myVRP,neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode:Int) = onePointMove(
    () => List(lastNode),
    ()=> myVRP.kFirst(v*2,
      ChainsHelper.relevantNeighborsForLastNodeAfterHead(
        myVRP,
        chainsExtension,
        Some(relevantPredecessorsOfNodes(lastNode))),
      postFilter),
    myVRP,
    neighborhoodName = "MoveLastOfChain")

  val oneChainMove = {
    profile(dynAndThen(firstNodeOfChainMove,
      (moveMove: OnePointMoveMove) => {
        mu[OnePointMoveMove, Option[List[Int]]](
          lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
          nextMoveGenerator,
          None,
          Int.MaxValue,
          false)
      })name "OneChainMove")
  }

  def onePtMove(k:Int) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,closestRelevantPredecessorsByDistance,postFilter), myVRP))

  def segExchangeOnSegments(k: Int) = profile(
    segmentExchangeOnSegments(myVRP,
      () => Array.tabulate(v)(vehicle => vehicle -> ChainsHelper.computeCompleteSegments(myVRP,vehicle,chainsExtension)).toMap,
      ()=> closestRelevantPredecessorsByDistance,
      () => 0 until v,
      selectFirstSegmentBehavior = Best(),
      selectSecondSegmentBehavior = Best(),
      selectFirstVehicleBehavior = Best(),
      selectSecondVehicleBehavior = Best()
    ))

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
            () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP,chainsExtension), myVRP)
          Some(insertNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainInsertion = insertPointUnroutedFirst(() => myVRP.unrouted.value.filter(chainsExtension.isHead),()=> {
    myVRP.kFirst(v*2,closestRelevantPredecessorsByDistance, postFilter)
  }, myVRP,neighborhoodName = "InsertUF")

  def lastNodeOfChainInsertion(lastNode:Int) = insertPointUnroutedFirst(
    () => List(lastNode),
    ()=> myVRP.kFirst(
      v*2,
      ChainsHelper.relevantNeighborsForLastNodeAfterHead(
        myVRP,
        chainsExtension,
        Some(relevantPredecessorsOfNodes(lastNode))),
      postFilter),
    myVRP,
    neighborhoodName = "InsertUF")

  val oneChainInsert = {
    profile(dynAndThen(firstNodeOfChainInsertion,
      (insertMove: InsertPointMove) => {
        mu[InsertPointMove,Option[List[Int]]](
          lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
          nextInsertGenerator,
          None,
          Int.MaxValue,
          false)
      })name "OneChainInsert")

  }

  //val routeUnroutedPoint =  Profile(new InsertPointUnroutedFirst(myVRP.unrouted,()=> myVRP.kFirst(10,filteredClosestRelevantNeighborsByDistance), myVRP,neighborhoodName = "InsertUF"))


  val search = bestSlopeFirst(List(oneChainInsert,oneChainMove,segExchangeOnSegments(5),onePtMove(20)))
  //val search = (BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, vlsn1pt)))


  search.verbose = 1
  //search.verboseWithExtraInfo(4, ()=> "" + myVRP)



  search.doAllMoves(obj=obj)

  println(myVRP)

  println(search.profilingStatistics)
}
