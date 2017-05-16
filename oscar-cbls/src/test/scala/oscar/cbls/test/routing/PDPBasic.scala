package oscar.cbls.test.routing

import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.business.routing.model.{ClosestNeighbors, PDP}
import oscar.cbls.core.computation.{CBLSSetConst, Store}
import oscar.cbls.core.objective.{CascadingObjective, Objective}
import oscar.cbls.lib.invariant.routing.{ConstantRoutingDistance, PDPConstraints}
import oscar.cbls.lib.invariant.seq.{Content, Size}
import oscar.cbls.lib.invariant.set.Diff
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Mu, Profile}
import oscar.cbls.modeling.Algebra._

import scala.collection.immutable.SortedSet

/**
  * Created by fg on 12/05/17.
  */

class MySimplePickupAndDeliveryProblem(n: Int, v: Int, m: Store,
                                       symmetricDistance: Array[Array[Int]],
                                       chains: List[List[Int]],
                                       contentsFlow: Array[Int],
                                       vehiclesCapacities:Array[Int]) extends PDP(n, v, m, chains) with ClosestNeighbors{
  override protected def getDistance(from: Int, to: Int): Int = symmetricDistance(from)(to)

  val penaltyForUnrouted = 1000000

  val timeWindows = Array.tabulate(17)(_ => (0,Int.MaxValue,0,Int.MaxValue))

  defineContentsFlow(contentsFlow)
  setVehicleMaxCapacities(vehiclesCapacities)
  addTimeWindows(timeWindows)

  val constraints = PDPConstraints(this)


  val routed = Content(routes.createClone(50)).setName("routed nodes")
  val unrouted = Diff(CBLSSetConst(SortedSet(nodes:_*)),routed).setName("unrouted nodes")

  val totalDistance = ConstantRoutingDistance(routes, n, v ,false, symmetricDistance, true)(0)

  val obj = new CascadingObjective(constraints,new CascadingObjective(constraints, totalDistance + (penaltyForUnrouted*(n - Size(routes)))))
}

object PDPBasic extends App{
  val m = new Store(noCycle = false)
  val v = 1
  val vehiclesCapacities = Array(4)
  val chains = List(List(1,2,3,4), List(5,6), List(7,8,9), List(10,11), List(12,13), List(14,15,16))
  val n = chains.map(_.length).sum + v
  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1

  val contentsFlow = Array(0,1,1,-1,-1,2,-2,1,2,-3,1,-1,1,-1,1,0,-1)

  val pdp =  new MySimplePickupAndDeliveryProblem(n,v,m,symmetricDistance,chains, contentsFlow, vehiclesCapacities)

  m.close()

  println(pdp.chainDico)

  // REMOVING

  def firstRemove(firstNode: Option[List[Int]] = None) = RemovePoint(() => firstNode.getOrElse(pdp.routedPickups), pdp)

  val nextRemoveGenerator =
    (exploredMoves:List[RemovePointMove], t:Option[List[Int]]) => {
      val chainTail: List[Int] = t match {
        case None => val removedPoint = exploredMoves.head.pointToRemove
          pdp.chainDico(removedPoint)
        case Some(tail:List[Int]) => tail
      }

      chainTail match {
        case Nil => None
        case nextPointToRemove :: newTail =>
          val removeNeighborhood = RemovePoint(() => Some(nextPointToRemove), pdp)
          Some(removeNeighborhood, Some(newTail))
      }
    }

  val removeChains = Mu[RemovePointMove, Option[List[Int]]](
    firstRemove(),
    nextRemoveGenerator,
    None,
    intermediaryStops = false,
    maxDepth = Int.MaxValue
  )


  // MOVING

  def firstMove(firstNode: Option[List[Int]] = None) = OnePointMove(() => firstNode.getOrElse(pdp.routedPickups), () => pdp.kFirst(15,pdp.closestNeighboursForward,pdp.isRouted), pdp)

  val nextMoveGenerator =
    (exploredMoves:List[OnePointMoveMove], t:Option[List[Int]]) => {
      val chainTail: List[Int] = t match {
        case None => val movedPoint = exploredMoves.head.movedPoint
          pdp.chainDico(movedPoint)
        case Some(tail:List[Int]) => tail
      }

      chainTail match {
        case Nil => None
        case nextPointToMove :: newTail =>
          val moveNeighborhood = OnePointMove(() => Some(nextPointToMove),
            () => pdp.relevantNewPredecessorsOf(), pdp)
          Some(moveNeighborhood, Some(newTail))
      }
    }

  val moveChains = Mu[OnePointMoveMove, Option[List[Int]]](
    firstMove(),
    nextMoveGenerator,
    None,
    intermediaryStops = false,
    maxDepth = Int.MaxValue
  )


  // INSERTING

  def firstInsert(firstNode: Option[List[Int]] = None) = InsertPointUnroutedFirst(() => firstNode.getOrElse(pdp.unroutedPickups),
    () => pdp.kFirst(15,pdp.closestNeighboursForward,pdp.isRouted),pdp)

  val nextInsertGenerator =
    (exploredMoves:List[InsertPointMove], t:Option[List[Int]]) => {
      val chainTail: List[Int] = t match {
        case None => val insertedPickup = exploredMoves.head.insertedPoint
          pdp.chainDico(insertedPickup)
        case Some(tail:List[Int]) => tail
      }

      chainTail match {
        case Nil => None
        case nextPointToInsert :: newTail =>
          val insertNeighborhood = InsertPointUnroutedFirst(() => Some(nextPointToInsert),
            () => pdp.kFirst(15, pdp.closestNeighboursForward, pdp.isRouted), pdp)
          Some(insertNeighborhood, Some(newTail))
      }
    }

  val insertChains = Mu[InsertPointMove, Option[List[Int]]](
    firstInsert(),
    nextInsertGenerator ,
    None,
    intermediaryStops = false, maxDepth = Int.MaxValue)


  val search = BestSlopeFirst(List(insertChains,moveChains))
  //val search = (BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, vlsn1pt)))


  search.verbose = 2
  //search.verboseWithExtraInfo(4, ()=> "" + pdp)

  search.doAllMoves(obj=pdp.obj)

  println(pdp)

  search.profilingStatistics
}
