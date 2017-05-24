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

import oscar.cbls.business.routing.model.ClosestNeighbors
import oscar.cbls.business.routing.model.RoutedAndUnrouted
import oscar.cbls.business.routing.model.TotalConstantDistance
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.neighborhood.InsertPointRoutedFirst
import oscar.cbls.business.routing.neighborhood.InsertPointUnroutedFirst
import oscar.cbls.business.routing.neighborhood.OnePointMove
import oscar.cbls.business.routing.neighborhood.OnePointMoveMove
import oscar.cbls.business.routing.neighborhood.ThreeOpt
import oscar.cbls.business.routing.neighborhood.TwoOpt1
import oscar.cbls.core.computation.{CBLSIntVar, Store}
import oscar.cbls.core.objective.{CascadingObjective, Objective}
import oscar.cbls.core.propagation.ErrorChecker
import oscar.cbls.core.propagation.ErrorChecker
import oscar.cbls.lib.constraint.LE
import oscar.cbls.lib.invariant.routing.{ForwardCumulativeConstraintOnVehicle, RouteSuccessorAndPredecessors}
import oscar.cbls.lib.invariant.seq.Size
import oscar.cbls.lib.invariant.seq.Size
import oscar.cbls.lib.search.combinators.BestSlopeFirst
import oscar.cbls.lib.search.combinators.Mu
import oscar.cbls.lib.search.combinators.Profile
import oscar.cbls.modeling.Algebra._
import oscar.cbls.business.routing.model._
import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.lib.search.combinators.{Mu, BestSlopeFirst, Profile}

class MySimpleRoutingWithCumulatives(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int, deltaAtNode:Array[Int], maxCapa:Int)
  extends VRP(n,v,m,maxPivot) with TotalConstantDistance with ClosestNeighbors with RoutedAndUnrouted{

  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  val penaltyForUnrouted  = 10000

  val maxNodes = LE(Size(routes),5).violation


  val violation = new CBLSIntVar(routes.model, 0, 0 to Int.MaxValue, "violation of capacity test")

  val contentConstraint = new ForwardCumulativeConstraintOnVehicle(
  routes,
  n,
  v,
  {case (fromNode,toNode,content) => content + deltaAtNode(fromNode)},
  maxCapa,
  Array.tabulate(v)(deltaAtNode),
  violation,
  6,
  6)

  val obj = new CascadingObjective(
    contentConstraint.violation,
    new CascadingObjective(maxNodes,
      Objective(totalDistance + (penaltyForUnrouted*(n - Size(routes))))))

  this.addToStringInfo(() => "objective: " + obj.value)
  this.addToStringInfo(() => "n:" + n + " v:" + v)

  val closestNeighboursForward = computeClosestNeighborsForward()

  def size = routes.value.size

  //TODO: how about using NextAndPRev trait? or use a clone of route?
  val (next,prev) = RouteSuccessorAndPredecessors(routes,v,n)

  this.addToStringInfo(() => "next: [" + next.map(_.value).mkString(",") + "]")
  this.addToStringInfo(() => "prev: [" + prev.map(_.value).mkString(",") + "]")
  this.addToStringInfo(() => "content: [" + contentConstraint.contentOfVehicle.mkString(",") + "]")
}

object TestCumulatives extends App{

  val n = 10
  val v = 2
  val delta = Array(0,1,-1,2,-2,3,-3,4,-4,0)
  val maxPivotPerValuePercent = 4

  println("VRP(n:" + n + " v:" + v + ")")

  val (symmetricDistanceMatrix,pointsList) = RoutingMatrixGenerator(n)
  //  println("restrictions:" + restrictions)
  val model = new Store(checker = Some(new ErrorChecker()))

  val myVRP = new MySimpleRoutingWithCumulatives(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent,delta,5)

  model.close()

  def routeUnroutedPoint =  Profile(new InsertPointUnroutedFirst(myVRP.unrouted,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,neighborhoodName = "InsertUF",best=true))

  //TODO: using post-filters on k-nearest is probably crap
  val routeUnroutedPoint2 =  Profile(new InsertPointRoutedFirst(myVRP.routed,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,x => !myVRP.isRouted(x)),myVRP,neighborhoodName = "InsertRF")  guard(() => myVRP.size < n/2))

  def onePtMove(k:Int) = Profile(new OnePointMove(myVRP.routed, () => myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP))

  val twoOpt = Profile(new TwoOpt1(myVRP.routed, ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP))

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(myVRP.routed, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val vlsn1pt = Profile(Mu[OnePointMoveMove](
    OnePointMove(myVRP.routed, () => myVRP.kFirst(5,myVRP.closestNeighboursForward,myVRP.isRouted),myVRP),
    l => Some(OnePointMove(() => List(l.head.newPredecessor).filter(_ >= v), () => myVRP.kFirst(3,myVRP.closestNeighboursForward,myVRP.isRouted),myVRP, hotRestart = false)),
    intermediaryStops = true,
    maxDepth = 6))

  val remove = RemovePoint(() => myVRP.routed.value.filter(_>=v), myVRP)

  val swapInOut = Profile((remove andThen routeUnroutedPoint) name ("SWAP"))
  val search = (BestSlopeFirst(List(routeUnroutedPoint2, routeUnroutedPoint, swapInOut, onePtMove(10),twoOpt, threeOpt(10,true),vlsn1pt, routeUnroutedPoint)) exhaust threeOpt(20,true))// afterMove(/*myVRP.drawRoutes()*/)

  // val search = (new RoundRobin(List(routeUnroutdPoint2,onePtMove(10) guard (() => myVRP.unrouted.value.size != 0)),10)) exhaust BestSlopeFirst(List(onePtMove(20),twoOpt, threeOpt(10,true))) exhaust threeOpt(20,true)

  search.verbose = 1
  //search.verboseWithExtraInfo(1, ()=> "" + myVRP)

  print("Doing all moves ...")


  search.doAllMoves(obj = myVRP.obj)
  model.propagate()
  println(search.profilingStatistics)

  println(myVRP)
}
