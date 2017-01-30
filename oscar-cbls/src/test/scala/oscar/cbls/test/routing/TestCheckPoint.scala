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


import oscar.cbls.core.computation.Store
import oscar.cbls.core.propagation.ErrorChecker
import oscar.cbls.lib.invariant.routing.RouteSuccessorAndPredecessors
import oscar.cbls.lib.invariant.seq.Size
import oscar.cbls.modeling.Algebra._
import oscar.cbls.core.objective.Objective
import oscar.cbls.business.routing.model._
import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, DynAndThen, Profile}

/**
  * Created by f.germeau on 07/09/2016.
  */
class MyVRP(n:Int,v:Int,model:Store,symmetricDistanceMatrix:Array[Array[Int]]) extends VRP(n,v,model)
  with TotalConstantDistance with ClosestNeighbors with RoutedAndUnrouted with NextAndPrev{

  setSymmetricDistanceMatrix(symmetricDistanceMatrix)

  override protected def getDistance(from: Int, to: Int): Int = symmetricDistanceMatrix(from)(to)

  val penaltyForUnrouted = 10000

  val obj = Objective(totalDistance + (penaltyForUnrouted*(n - Size(routes))))

  val closestNeighborsForward = computeClosestNeighborsForward()
}

object TestCheckPoint extends App{

  val n = 500
  val v = 10
  val maxMovesBeforeRollBack = 100

  println("VRP(n:" + n + " v:" + v + ")")

  val (symmetricDistanceMatrix,pointsList) = RoutingMatrixGenerator(n)

  val model = Store(noCycle = false, checker = Some(new ErrorChecker))

  val myVRP = new MyVRP(n,v,model,symmetricDistanceMatrix)

  model.close()

  val insertPoint = Profile(InsertPointUnroutedFirst(
    unroutedNodesToInsert = () => myVRP.unroutedNodes,
    relevantPredecessor = () => myVRP.kFirst(n/10,myVRP.closestNeighborsForward,myVRP.isRouted),
    vrp = myVRP
  ))

  val onePointMove = Profile(OnePointMove(
    nodesToMove = myVRP.routed,
    relevantNewPredecessors = () => myVRP.kFirst(n/10, myVRP.closestNeighborsForward,myVRP.isRouted),
    vrp = myVRP
  ))

  val twoOpt = Profile(TwoOpt1(
    segmentStartValues = myVRP.routed,
    relevantNewSuccessors = () => myVRP.kFirst(n/10, myVRP.closestNeighborsForward,myVRP.isRouted),
    vrp = myVRP
  ))

  val threeOpt = Profile(ThreeOpt(
    potentialInsertionPoints = myVRP.routed,
    relevantNeighbors = () => myVRP.kFirst(20, myVRP.closestNeighborsForward,myVRP.isRouted),
    vrp = myVRP
  ))

  val doubleInsert = Profile(DynAndThen(
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => myVRP.unroutedNodes,
      relevantPredecessor = () => myVRP.kFirst(n/10,myVRP.closestNeighborsForward,myVRP.isRouted),
      vrp= myVRP
    ), (moveResult:InsertPointMove)=>
      InsertPointUnroutedFirst(
        unroutedNodesToInsert = () => myVRP.unroutedNodes,
        relevantPredecessor = () => myVRP.kFirst(n/10,myVRP.closestNeighborsForward,myVRP.isRouted),
        vrp= myVRP
      )
  ))

  //For  test purpose, we create an additional checkpoint here.
  var checkPoint = myVRP.routes.defineCurrentValueAsCheckpoint(true)
  var movesBeforeRollBack = 1
  var moves = 0

  val search = Profile(BestSlopeFirst(List(insertPoint,onePointMove,twoOpt,threeOpt,doubleInsert))).afterMove({
    if(moves == movesBeforeRollBack && movesBeforeRollBack < maxMovesBeforeRollBack) {
      myVRP.routes.rollbackToTopCheckpoint(checkPoint)
      movesBeforeRollBack += 1
      moves = 0
    }
    moves += 1
  })

  search.verbose = 1
  //search.verboseWithExtraInfo(1, ()=> "" + myVRP)

  print("Doing all moves ...")


  search.doAllMoves(obj = myVRP.obj)
  model.propagate()
  println(search.profilingStatistics)
}
