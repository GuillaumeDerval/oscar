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
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

class MySimpleRoutingWithUnroutedPointsAndNext(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int, pointsList:Array[(Int,Int)] = null)
  extends VRP(n,v,m,maxPivot)
  with TotalConstantDistance with ClosestNeighbors with RoutedAndUnrouted
  //with RoutingMapDisplay
{

  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  val penaltyForUnrouted  = 10000

  val obj = Objective(totalDistance + (penaltyForUnrouted*(n - Size(routes))))

  this.addToStringInfo(() => "objective: " + obj.value)
  this.addToStringInfo(() => "n:" + n + " v:" + v)

  val closestNeighboursForward = computeClosestNeighborsForward()

  def size = routes.value.size

  //TODO: how about using NextAndPRev trait? or use a clone of route?
  val (next,prev) = RouteSuccessorAndPredecessors(routes,v,n)

  this.addToStringInfo(() => "next:" + next.map(_.value).mkString(","))
  this.addToStringInfo(() => "prev:" + prev.map(_.value).mkString(","))

  //if(pointsList != null)
    //initializeRoutingMap(Array.tabulate(n)(node => (pointsList(node)._1.toDouble, pointsList(node)._2.toDouble)),this,1000)


}

object TestNext extends App{

  val n = 2000
  val v = 10

  val maxPivotPerValuePercent = 4

  println("VRP(n:" + n + " v:" + v + ")")

  val (symmetricDistanceMatrix,pointsList) = RoutingMatrixGenerator(n)
  //  println("restrictions:" + restrictions)
  val model = new Store(checker = Some(new ErrorChecker()))

  val myVRP = new MySimpleRoutingWithUnroutedPointsAndNext(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent,pointsList)

  model.close()

  val routeUnroutedPoint =  Profile(new InsertPointUnroutedFirst(myVRP.unrouted,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,neighborhoodName = "InsertUF"))

  //TODO: using post-filters on k-nearest is probably crap
  val routeUnroutedPoint2 =  Profile(new InsertPointRoutedFirst(myVRP.routed,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,x => !myVRP.isRouted(x)),myVRP,neighborhoodName = "InsertRF")  guard(() => myVRP.size < n/2))

  def onePtMove(k:Int) = Profile(new OnePointMove(myVRP.routed, () => myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP))

  val twoOpt = Profile(new TwoOpt1(myVRP.routed, ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP))

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(myVRP.routed, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val search = (BestSlopeFirst(List(routeUnroutedPoint2, routeUnroutedPoint, onePtMove(10),twoOpt, threeOpt(10,true))) exhaust threeOpt(20,true))// afterMove(/*myVRP.drawRoutes()*/)

 // val search = (new RoundRobin(List(routeUnroutdPoint2,onePtMove(10) guard (() => myVRP.unrouted.value.size != 0)),10)) exhaust BestSlopeFirst(List(onePtMove(20),twoOpt, threeOpt(10,true))) exhaust threeOpt(20,true)

  search.verbose = 1
  //search.verboseWithExtraInfo(1, ()=> "" + myVRP)

  print("Doing all moves ...")


  search.doAllMoves(obj = myVRP.obj)
  model.propagate()
  println(search.profilingStatistics)
}
