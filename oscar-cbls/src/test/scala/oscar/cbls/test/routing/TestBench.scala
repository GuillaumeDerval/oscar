package oscar.cbls.test.routing

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

import oscar.cbls.core.search.{MoveFound, NoMoveFound}

import scala.math.pow
import scala.math.round
import scala.math.sqrt

import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.prop.Checkers

import oscar.cbls.core.computation.SetValue.toFunction
import oscar.cbls.core.computation.Store
import oscar.cbls.core.objective.Objective
import oscar.cbls.business.routing.legacy.model.ClosestNeighbors
import oscar.cbls.business.routing.legacy.model.HopClosestNeighbors
import oscar.cbls.business.routing.legacy.model.HopDistanceAsObjectiveTerm
import oscar.cbls.business.routing.legacy.model.PenaltyForUnrouted
import oscar.cbls.business.routing.legacy.model.PositionInRouteAndRouteNr
import oscar.cbls.business.routing.legacy.model.Predecessors
import oscar.cbls.business.routing.legacy.model.RoutedAndUnrouted
import oscar.cbls.business.routing.legacy.model.UnroutedImpl
import oscar.cbls.business.routing.legacy.model.VRP
import oscar.cbls.business.routing.legacy.model.VRPObjective
import oscar.cbls.business.routing.legacy.neighborhood.InsertPointUnroutedFirst
import oscar.cbls.business.routing.legacy.neighborhood.OnePointMove
import oscar.cbls.business.routing.legacy.neighborhood.OnePointMoveMove
import oscar.cbls.business.routing.legacy.neighborhood.Swap
import oscar.cbls.business.routing.legacy.neighborhood.SwapMove
import oscar.cbls.business.routing.legacy.neighborhood.ThreeOpt
import oscar.cbls.business.routing.legacy.neighborhood.ThreeOptMove
import oscar.cbls.business.routing.legacy.neighborhood.TwoOpt
import oscar.cbls.business.routing.legacy.neighborhood.TwoOptMove

object RandomInsert {
  /**
   * It applies the initial solution to a given vrp problem.
   * @param vrp : the vrp problem that we want to apply the initial solution.
   */
  def apply(vrp: VRP with RoutedAndUnrouted with VRPObjective with PositionInRouteAndRouteNr) {
    print("Applying random insert heuristic...")
    InsertPointUnroutedFirst(vrp.unrouted,
      () => (n: Int) => vrp.routed.value,
      vrp: VRP,
      "Random insert",
      best = false,
      hotRestart = true).doAllMoves(obj = vrp.getObjective,
        acceptanceCriterion = (oldObj, newObj) =>
          if (newObj == Int.MaxValue) false else math.random < 0.5)
    println(" done.")
  }
}

object TestBench extends FunSuite with Matchers with Checkers {

  def onePoint(best: Boolean = false)(f: MoveFixture): Boolean = {
    val nodesPrecedingNodesToMove = () => f.vrp.nodes
    val relevantNeighbors = () => (n: Int) => f.vrp.nodes
    println("VRP before the search: " + f.vrp)

    OnePointMove(nodesPrecedingNodesToMove,
      relevantNeighbors,
      f.vrp,
      best = best,
      hotRestart = false).getMove(f.vrp.getObjective,
        (oldVal, newVal) => oldVal > newVal) match {
          case MoveFound(m) => {
            m.isInstanceOf[OnePointMoveMove] should be(true)
            val move = m.asInstanceOf[OnePointMoveMove]
            println("An improving move was found ! : " + move)
            val movedPoint = f.vrp.next(move.predOfMovedPoint).value
            println("movedPoint=" + movedPoint)
            val nextPoint = f.vrp.next(movedPoint).value
            println("nextPoint=" + nextPoint)
            val destPoint = f.vrp.next(move.insertionPoint).value
            println("destPoint=" + destPoint)
            m.commit
            println("VRP after the move: " + f.vrp)
            f.mainRouteLength should be(f.initLength)
            f.vrp.routes.positionInRoute(0).value should be(0)
            for (i <- f.vrp.nodes) {
              if (i == move.predOfMovedPoint)
                f.vrp.next(i).value should be(nextPoint)
              else if (i == move.insertionPoint)
                f.vrp.next(i).value should be(movedPoint)
              else if (i == movedPoint)
                f.vrp.next(i).value should be(destPoint)
              else
                f.vrp.next(i).value should be(f.initNext(i))
            }
            true
          }
          case NoMoveFound => false
        }
  }

  def swap(best: Boolean = false)(f: MoveFixture): Boolean = {
    val pos = f.vrp.nodes.map((n: Int) => f.vrp.routes.positionInRoute(n).value)
    val initLength = f.mainRouteLength
    val nodesPrecedingNodesToMove = () => f.vrp.nodes
    val relevantNeighbors = () => (n: Int) => f.vrp.nodes
    println("VRP before the search: " + f.vrp)

    Swap(nodesPrecedingNodesToMove,
      relevantNeighbors,
      f.vrp,
      best = best,
      hotRestart = false).getMove(f.vrp.getObjective) match {
        case MoveFound(m) => {
          m.isInstanceOf[SwapMove] should be(true)
          val swap = m.asInstanceOf[SwapMove]
          println("An improving move was found ! : " + swap)
          val fst = f.vrp.next(swap.fstPred).value
          val snd = f.vrp.next(swap.sndPred).value
          val fstNext = f.vrp.next(fst).value
          val sndNext = f.vrp.next(snd).value
          m.commit

          println("VRP after the move: " + f.vrp)
          f.mainRouteLength should be(f.initLength)
          f.vrp.routes.positionInRoute(0).value should be(0)
          for (i <- f.vrp.nodes) {
            if (i == swap.fstPred)
              f.vrp.next(i).value should be(snd)
            else if (i == snd)
              f.vrp.next(i).value should be(fstNext)
            else if (i == swap.sndPred)
              f.vrp.next(i).value should be(fst)
            else if (i == fst)
              f.vrp.next(i).value should be(sndNext)
            else
              f.vrp.next(i).value should be(f.initNext(i))
          }
          true
        }
        case NoMoveFound => false
      }
  }

  def twoOpt(best: Boolean = false)(f: MoveFixture) = {
    val predecesorOfFirstMovedPoint = () => f.vrp.nodes
    val relevantNeighbors = () => (n: Int) => f.vrp.nodes
    println("VRP before the search: " + f.vrp)

    TwoOpt(predecesorOfFirstMovedPoint,
      relevantNeighbors,
      f.vrp,
      best = best,
      hotRestart = true).getMove(f.vrp.getObjective) match {
        case MoveFound(m) => {
          m.isInstanceOf[TwoOptMove] should be(true)
          val move = m.asInstanceOf[TwoOptMove]
          println("An improving move was found ! : " + move)
          val segStart = f.initNext(move.fstPred)
          val sndEdgeEnd = f.initNext(move.sndPred)
          m.commit

          println("VRP after the move: " + f.vrp)
          f.mainRouteLength should be(f.initLength)
          f.vrp.routes.positionInRoute(0).value should be(0)
          for (i <- f.vrp.nodes) {
            if (i == move.fstPred)
              f.vrp.next(i).value should be(move.sndPred)
            else if (i == segStart)
              f.vrp.next(i).value should be(sndEdgeEnd)
            else if (f.isInSeg(i, segStart, move)) f.vrp.nodes.find(f.initNext(_) == i) match {
              case None => assert(false, "This case should not occur.")
              case Some(initPred) => f.vrp.next(i).value should be(initPred)
            }
            else {
              f.vrp.next(i).value should be(f.initNext(i))
            }
          }
          true
        }
        case NoMoveFound => false
      }
  }

  def threeOpt(best: Boolean = false, kk: Boolean = false)(f: MoveFixture) = {
    val potentialInsertionPoints = () => f.vrp.nodes
    val relevantNeighbors = () => (n: Int) => f.vrp.nodes
    println("VRP before the search: " + f.vrp)

    ThreeOpt(potentialInsertionPoints,
      relevantNeighbors,
      f.vrp,
      best = best,
      hotRestart = false,
      KKIterationScheme = kk).getMove(f.vrp.getObjective,
        (oldVal, newVal) => oldVal > newVal) match {
          case MoveFound(m) => {
            m.isInstanceOf[ThreeOptMove] should be(true)
            val move = m.asInstanceOf[ThreeOptMove]
            println("An improving move was found ! : " + move)
            val segStartPoint = f.initNext(move.beforeStart)
            val afterEnd = f.initNext(move.segEndPoint)
            val afterInsertion = f.initNext(move.insertionPoint)
            m.commit

            println("VRP after the move: " + f.vrp)

            withClue("Main route length should not be modified:") {
              f.mainRouteLength should be(f.initLength)
            }
            withClue("Node 0 position in route should not be modified:") {
              f.vrp.routes.positionInRoute(0).value should be(0)
            }
            if (!move.reverseSegment) {
              for (i <- f.vrp.nodes) {
                if (i == move.beforeStart)
                  withClue("Initial segment end point should follow before start point:") {
                    f.vrp.next(i).value should be(f.initNext(move.segEndPoint))
                  }
                else if (i == move.segEndPoint)
                  withClue("Initial insertion point should follow segment end point:") {
                    f.vrp.next(i).value should be(f.initNext(move.insertionPoint))
                  }
                else if (i == move.insertionPoint)
                  withClue("Initial before start point should follow insertion point:") {
                    f.vrp.next(i).value should be(f.initNext(move.beforeStart))
                  }
                else
                  withClue("Any other node should keep the same following one:") {
                    f.vrp.next(i).value should be(f.initNext(i))
                  }
              }
            } else {
              for (i <- f.vrp.nodes) {
                if (i == move.beforeStart)
                  withClue("Initial segment end point should follow before start point:") {
                    f.vrp.next(i).value should be(f.initNext(move.segEndPoint))
                  }
                else if (i == segStartPoint)
                  withClue("Initial after insertion point should follow initial segment start point:") {
                    f.vrp.next(i).value should be(afterInsertion)
                  }
                else if (i == move.insertionPoint)
                  withClue("Initial second end point should follow insertion point:") {
                    f.vrp.next(i).value should be(move.segEndPoint)
                  }
                else if (f.vrp.isBetween(i, move.segEndPoint, f.initNext(move.beforeStart)))
                  withClue("Any other node of the segment should follow its initial following one:") {
                    f.vrp.next(i).value should be(f.initPred(i))
                  }
                else if (f.vrp.isBetween(i, afterInsertion, move.beforeStart))
                  withClue("Any other node should keep the same following one:") {
                    f.vrp.next(i).value should be(f.initNext(i))
                  }
              }
            }
            true
          }
          case NoMoveFound => false
        }
  }

  def checkUnrouted(f: MoveFixture, l: List[Int]) = l.foreach {
    (n: Int) =>
      f.vrp.routes.routeNr(n).value should be(f.ROUTE_ARRAY_UNROUTED)
      f.vrp.routes.positionInRoute(n).value should be(f.nbNodes)
  }
}

/**
 * Use this to write tests of isolated routing movement.
 */
class MoveFixture(
  val verbose: Int = 0,
  val randomWeight: Boolean = false,
  val nbNodes: Int = 10,
  val nbVehicules: Int = 1,
  var abscissa: Array[Int] = null,
  var ordinate: Array[Int] = null,
  val init: VRP with RoutedAndUnrouted with VRPObjective with PositionInRouteAndRouteNr with ClosestNeighbors => Unit) { // = BestInsert.apply) {

  val ROUTE_ARRAY_UNROUTED = 1

  if (abscissa == null)
    abscissa = Array.iterate(0, nbNodes)(_ + 1)
  if (ordinate == null)
    ordinate =
      if (randomWeight) {
        Gen.containerOfN[Array, Int](nbNodes, Gen.choose(0, 100)).sample.get
      } else {
        Array.fill(nbNodes)(0)
      }
  val matrix = getDistanceMatrix(abscissa, ordinate)
  val model = Store(false, None, false, false)

  val vrp = new VRP(nbNodes, nbVehicules, model) with Predecessors with HopDistanceAsObjectiveTerm with PositionInRouteAndRouteNr with HopClosestNeighbors with UnroutedImpl with PenaltyForUnrouted
  //with EasyRoutingNeighborhood

  vrp.addObjectiveTerm(vrp.unroutedPenalty)
  vrp.setUnroutedPenaltyWeight(10000)
  vrp.installCostMatrix(matrix)
  model.close()

  if (verbose > 0) {
    println
    if (verbose > 1) {
      println("Initial problem: " + vrp)
    }
  }

  init(vrp)
  // 0 -> 1 -> 2 -> ... -> nbNodes - 1 (-> 0)
  // or
  // 0 -> nbNodes - 1 -> ... -> 2 -> 1 (-> 0)
  model.propagate()

  val initLength = mainRouteLength
  val initNext = vrp.nodes.map((n: Int) => vrp.next(n).value)
  val initPred = vrp.nodes.map((n: Int) => vrp.preds(n).value)
  val initPos = vrp.nodes.map((n: Int) => vrp.routes.positionInRoute(n).value)
  def mainRouteLength = vrp.routes.routeLength(0).value

  /**
   * Gives a distance matrix by entering the abscissa and
   * ordinates of points in the plan.
   */
  def getDistanceMatrix(abscissa: Array[Int], ordinate: Array[Int]): Array[Array[Int]] = {
    val N = abscissa.length
    Array.tabulate(N, N)((i, j) => round(sqrt((pow(abscissa(i) - abscissa(j), 2)
      + pow(ordinate(i) - ordinate(j), 2)).toFloat)).toInt)
  }

  def genNodeOpt(constraint: Int => Boolean = (_: Int) => true): Option[Int] = {
    (Gen.choose(0, nbNodes - 1) suchThat constraint).sample
  }

  def genNodeToCutAfter(constraint: Int => Boolean = (_: Int) => true) = {
    def isNotADepotNextOf = (x: Int) => !vrp.isADepot(vrp.next(x).value)
    def aux: Int =
      (Gen.choose(0, nbNodes - 1)
        suchThat { (n: Int) => isNotADepotNextOf(n) && constraint(n) }).sample match {
          case Some(index) => index
          case None => aux
        }
    aux
  }

  def genSegToCut: (Int, Int) = {
    val beforeStart = genNodeOpt().get
    genNodeOpt(
      (n: Int) => n != vrp.next(beforeStart).value
        && (!vrp.isInstanceOf[PositionInRouteAndRouteNr]
          || vrp.asInstanceOf[PositionInRouteAndRouteNr].isASegment(beforeStart, n))) match {
        case Some(end) => (beforeStart, end)
        case None => genSegToCut
      }
  }

  def segLength(start: Int, end: Int) = {
    def aux(start: Int, length: Int): Int = {
      if (start == end) length
      else aux(vrp.next(start).value, length + 1)
    }
    aux(start, 1)
  }

  def segNodes(start: Int, end: Int) = {
    def aux(start: Int, nodes: List[Int]): List[Int] = {
      if (start == end) (start :: nodes).reverse
      else aux(vrp.next(start).value, start :: nodes)
    }
    aux(start, Nil)
  }

  /**
   * To check points from next(segStart) to sndPred
   */
  def isInSeg(i: Int, segStart: Int, move: TwoOptMove): Boolean = {
    var cur = segStart
    while (cur != move.sndPred) {
      cur = initNext(cur)
      if (i == cur) return true
    }
    false
  }
}