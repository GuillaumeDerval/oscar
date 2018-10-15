package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.TimeWindowConstraint
import oscar.cbls.lib.invariant.seq.Precedence
//import oscar.cbls.business.routing.invariants.{PDPConstraints, TimeWindowConstraint}
import oscar.cbls.core.search.Best

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
  var (earlylines, deadlines, taskDurations, maxWaitingDurations) = RoutingMatrixGenerator.generateFeasibleTimeWindows(n,v,travelDurationMatrix,listOfChains)
  deadlines = deadlines.take(v).map(x => Math.min(deadlines.drop(v).max*2, Int.MaxValue*9/10)) ++ deadlines.drop(v)

  val myVRP =  new VRP(m,n,v)

  // Distance
  val totalRouteLength = constantRoutingDistance(myVRP.routes,n,v,false,symmetricDistance,true,true,false)(0)

  val timeWindowExtension = timeWindow(earlylines,deadlines,taskDurations,maxWaitingDurations)

  val violations2 = Array.fill(v)(new CBLSIntVar(m, 0, Domain.coupleToDomain((0,1))))
  val timeWindowInvariant2 = TimeWindowConstraint(myVRP.routes, n, v, timeWindowExtension, travelDurationMatrix, violations2)

  //Chains
  val precedenceInvariant = new Precedence(myVRP.routes,precedences)
  val chainsExtension = chains(myVRP,listOfChains)

  //Next & prev
  val routeForNextPrev = myVRP.routes.createClone()
  val (next,prev) = (Array.tabulate(routeForNextPrev.maxValue + 1)(node =>
    CBLSIntVar(m,n,name="successor of node" + node)),
    Array.tabulate(routeForNextPrev.maxValue + 1)(node =>
      CBLSIntVar(m,n,name="predecessor of node" + node)))
  routeSuccessorAndPredecessors(routeForNextPrev,v,n)(next,prev)

  //Constraints & objective
  val obj = new CascadingObjective(precedenceInvariant,
    new CascadingObjective(sum(violations2),
      totalRouteLength + (penaltyForUnrouted*(n - length(myVRP.routes)))))

  m.close()

  val relevantPredecessorsOfNodes = TimeWindowHelper.relevantPredecessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)
  val relevantSuccessorsOfNodes = TimeWindowHelper.relevantSuccessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)
  val isARelevantSuccessorsOfNodes = Array.tabulate(n)(node => Array.tabulate(n)(x => relevantSuccessorsOfNodes(node).contains(x)))

  def postFilter(node:Int): (Int) => Boolean = {
    (neighbor: Int) => {
      val successor = next(neighbor).value
      myVRP.isRouted(neighbor) &&
        isARelevantSuccessorsOfNodes(node)(successor)
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
    ()=> myVRP.kFirst(v*4,closestRelevantPredecessorsByDistance,postFilter), myVRP,neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode:Int) = onePointMove(
    () => List(lastNode),
    ()=> myVRP.kFirst(v*4,
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
    myVRP.kFirst(v*4,closestRelevantPredecessorsByDistance, postFilter)
  }, myVRP,neighborhoodName = "InsertUF")

  def lastNodeOfChainInsertion(lastNode:Int) = insertPointUnroutedFirst(
    () => List(lastNode),
    ()=> myVRP.kFirst(
      v*4,
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


  val search = bestSlopeFirst(List(oneChainInsert,oneChainMove,segExchangeOnSegments(5),onePtMove(20)))

  search.verbose = 1

  search.doAllMoves(obj=obj)

  println(myVRP)

  println(search.profilingStatistics)
}
