package oscar.examples.cbls.routing

import oscar.cbls.business.routing.model.extensions._
import oscar.cbls.business.routing._
import oscar.cbls._
import oscar.cbls.modeling.ModelingAPI
import oscar.cbls.core.computation.{CBLSIntConst, Store}
import oscar.cbls.core.objective.CascadingObjective
import oscar.cbls.lib.invariant.routing.PDPConstraints
import oscar.cbls.lib.invariant.seq.{Length, Precedence}
import oscar.cbls.lib.search.combinators.Profile

/**
  * Created by fg on 12/05/17.
  */

object SimpleVRPWithTimeWindow extends App{
  val m = new Store(noCycle = false/*, checker = Some(new ErrorChecker)*/)
  val v = 10
  val n = 1000
  val penaltyForUnrouted = 100000
  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val travelDurationMatrix = RoutingMatrixGenerator.generateLinearTravelTimeFunction(n,symmetricDistance)
  val precedences = RoutingMatrixGenerator.generatePrecedence(n,v,(n-v)/2).map(p => List(p._1,p._2))
  val (earlylines, deadlines, taskDurations, maxWaitingDurations) = RoutingMatrixGenerator.generateFeasibleTimeWindows(n,v,travelDurationMatrix,precedences)
  
  val myVRP =  new VRP(m,n,v)

  // Distance
  val routingDistance = constantRoutingDistance(myVRP.routes,n,v,false,symmetricDistance,true,true,false)
  val distanceExtension = new Distance(myVRP,symmetricDistance,routingDistance)

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
  val timeWindowExtension = new TimeWindow(myVRP,tiweWindowInvariant,earlylines,deadlines,taskDurations,maxWaitingDurations)

  //Chains
  val precedenceInvariant = new Precedence(myVRP.routes,precedences.map(p => (p.head,p.last)))
  val chainsExtension = new Chains(myVRP,precedences)
  println(precedences.map(_.map(earlylines)))

  //Constraints & objective
  val (fastConstrains,slowConstraints) = PDPConstraints(myVRP,
    timeWindow = Some(timeWindowExtension),
    timeWindowInvariant = Some(tiweWindowInvariant),
    precedences = Some(precedenceInvariant))
  val obj = new CascadingObjective(fastConstrains,
    new CascadingObjective(slowConstraints,
      distanceExtension.totalDistance + (penaltyForUnrouted*(n - Length(myVRP.routes)))))

  m.close()
  def postFilter(node:Int) = myVRP.generatePostFilters(node, myVRP.isRouted)
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(distanceExtension.computeClosestPathFromNeighbor(myVRP.preComputedRelevantNeighborsOfNodes))
  def filteredClosestRelevantNeighborsByDistance(node: Int) = closestRelevantNeighborsByDistance(node).filter(postFilter(node)(_))


  // MOVING


  val nextMoveGenerator = {
    (exploredMoves:List[OnePointMoveMove], t:Option[List[Int]]) => {
      val chainTail: List[Int] = t match {
        case None => {
          val movedNode = exploredMoves.head.movedPoint
          chainsExtension.nextNodesInChain(movedNode)
        }
        case Some(tail: List[Int]) => tail
      }

      chainTail match {
        case Nil => None
        case head :: Nil => None
        case nextNodeToMove :: newTail =>
          val moveNeighborhood = onePointMove(() => Some(nextNodeToMove),
            () => chainsExtension.computeRelevantNeighborsForInternalNodes(), myVRP)
          Some(moveNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainMove = onePointMove(() => chainsExtension.heads.filter(myVRP.isRouted),()=> myVRP.kFirst(v*2,filteredClosestRelevantNeighborsByDistance), myVRP,neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode:Int) = onePointMove(() => List(lastNode),()=> myVRP.kFirst(v*2,chainsExtension.computeRelevantNeighborsForLastNode), myVRP,neighborhoodName = "MoveLastOfChain")

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
      )

  }

  def onePtMove(k:Int) = Profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,filteredClosestRelevantNeighborsByDistance), myVRP))

  // INSERTING

  val nextInsertGenerator = {
    (exploredMoves:List[InsertPointMove], t:Option[List[Int]]) => {
      val chainTail: List[Int] = t match {
        case None => {
          val insertedNode = exploredMoves.head.insertedPoint
          chainsExtension.nextNodesInChain(insertedNode)
        }
        case Some(tail: List[Int]) => tail
      }

      chainTail match {
        case Nil => None
        case head :: Nil => None
        case nextNodeToInsert :: newTail =>
          val insertNeighborhood = insertPointUnroutedFirst(() => Some(nextNodeToInsert),
            () => chainsExtension.computeRelevantNeighborsForInternalNodes(), myVRP)
          Some(insertNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainInsertion = insertPointUnroutedFirst(() => chainsExtension.heads.filter(n => !myVRP.isRouted(n)),()=> myVRP.kFirst(10,filteredClosestRelevantNeighborsByDistance), myVRP,neighborhoodName = "InsertUF")

  def lastNodeOfChainInsertion(lastNode:Int) = insertPointUnroutedFirst(() => List(lastNode),()=> myVRP.kFirst(v*2,chainsExtension.computeRelevantNeighborsForLastNode), myVRP,neighborhoodName = "InsertUF")

  val oneChainInsert = {
    dynAndThen(firstNodeOfChainInsertion,
      (insertMove: InsertPointMove) => {
        println("Insert Mu " + insertMove.insertedPoint)
        mu[InsertPointMove,Option[List[Int]]](
          lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
          nextInsertGenerator,
          None,
          Int.MaxValue,
          false)
      })

  }

  //val routeUnroutedPoint =  Profile(new InsertPointUnroutedFirst(myVRP.unrouted,()=> myVRP.kFirst(10,filteredClosestRelevantNeighborsByDistance), myVRP,neighborhoodName = "InsertUF"))


  val search = bestSlopeFirst(List(oneChainInsert,oneChainMove,onePtMove(20)))
  //val search = (BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, vlsn1pt)))


  search.verbose = 2
  //search.verboseWithExtraInfo(4, ()=> "" + myVRP)



  search.doAllMoves(obj=obj)

  println(myVRP)
  val arrivalTimes = tiweWindowInvariant.content1AtNode.map(_.value).toList
  println(myVRP.routes.value.map(arrivalTimes).toList)
  println(earlylines.toList)
  println(deadlines.toList)
  println(taskDurations.toList)
  println(maxWaitingDurations.toList)

  search.profilingStatistics
}
