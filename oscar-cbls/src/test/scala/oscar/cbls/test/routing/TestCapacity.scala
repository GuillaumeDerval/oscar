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

import oscar.cbls._
import oscar.cbls.business.routing.model.{ClosestNeighbors, RoutedAndUnrouted, TotalConstantDistance, VRP}
import oscar.cbls.business.routing.neighborhood.{InsertPointRoutedFirst, InsertPointUnroutedFirst, OnePointMove, OnePointMoveMove, ThreeOpt, TwoOpt, _}
import oscar.cbls.core.objective.CascadingObjective
import oscar.cbls.core.search.{Best, First}
import oscar.cbls.business.routing.invariants.capa.ForwardCumulativeConstraintOnVehicle
import oscar.cbls.lib.invariant.seq.Length
import oscar.cbls.lib.search.combinators.{Mu, Profile, RoundRobin}


class RoutingWithCapacityMax(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int, deltaAtNode:Array[Int], maxCapa:Int)
  extends VRP(n,v,m,maxPivot) with TotalConstantDistance with ClosestNeighbors with RoutedAndUnrouted{

  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  val penaltyForUnrouted  = 10000

  val violation = new CBLSIntVar(routes.model, 0, 0 to Int.MaxValue, "violation of capacity test")

  val contentConstraint = new ForwardCumulativeConstraintOnVehicle(
  routes,
  n,
  v,
  {case (fromNode,toNode,content) =>
    val contentOut = content + deltaAtNode(toNode)
//    println("querying capacity(fromNode:" + fromNode + " toNode:" + toNode + " content:" + content + " contentOut:" + contentOut)
    contentOut
    },
  maxCapa,
  Array.tabulate(v)(deltaAtNode),
  violation,
  6,fullDebug=true)

  val violation2 = new CBLSIntVar(routes.model, 0, 0 to Int.MaxValue, "violation of capacity test2")
  val contentConstraint2 = new ForwardCumulativeConstraintOnVehicle(
  routes,
  n,
  v,
  {case (fromNode,toNode,content) =>
    val contentOut = content + deltaAtNode(toNode)
    //    println("querying capacity(fromNode:" + fromNode + " toNode:" + toNode + " content:" + content + " contentOut:" + contentOut)
    contentOut
  },
  maxCapa-1,
  Array.tabulate(v)(deltaAtNode),
  violation2,
  6,fullDebug=true)

  val obj = new CascadingObjective(
    contentConstraint.violation,
    totalDistance + (penaltyForUnrouted*(n - Length(routes))))

  val closestNeighboursForward = computeClosestNeighborsForward()

  def size = routes.value.size

  override def toString : String = super.toString +
    "objective: " + obj.detailedString(false) + "\n" +
    contentConstraint + contentConstraint2
}

object TestCapacity extends App{

  val n = 30
  val v = 5
  val delta = Array(0,1,1,2,2,3,-3,4,-4,0,0,1,-1,2,-2,3,-3,4,-4,0,0,1,-1,2,-2,3,-3,4,-4,0)
  val maxPivotPerValuePercent = 4
  val maxcapa = 5
  println("VRP(n:" + n + " v:" + v + ")")

  val (symmetricDistanceMatrix,pointsList) = RoutingMatrixGenerator(n)
  //  println("restrictions:" + restrictions)
  val model = new Store() //checker = Some(new ErrorChecker()))


  println("Delta At Nodes: " )
  for(node <- delta.indices){
    println("\tnode:" + node + "\tdelta:" + delta(node))
  }
  val myVRP = new RoutingWithCapacityMax(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent,delta,maxcapa)

  model.close()

  def routeUnroutedPoint(k:Int) =  new InsertPointUnroutedFirst(myVRP.unrouted,()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted),
    myVRP,neighborhoodName = "InsertUF",selectNodeBehavior = First(),selectInsertionPointBehavior = First())

  //TODO: using post-filters on k-nearest is probably crap
  val routeUnroutedPoint2 =  Profile(new InsertPointRoutedFirst(myVRP.routed,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,x => !myVRP.isRouted(x)),myVRP,neighborhoodName = "InsertRF")  guard(() => myVRP.size < n/2))

  def onePtMove(k:Int) = Profile(new OnePointMove(
    myVRP.routed,
    () => myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted),
    myVRP,
    selectPointToMoveBehavior = Best(),
    selectDestinationBehavior = Best()))

  val twoOpt = Profile(new TwoOpt(myVRP.routed, ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP))

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(myVRP.routed, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val vlsn1pt = Profile(Mu[OnePointMoveMove](
    OnePointMove(myVRP.routed, () => myVRP.kFirst(3,myVRP.closestNeighboursForward,myVRP.isRouted),myVRP, selectDestinationBehavior = Best(),selectPointToMoveBehavior = First()),
    l => Some(OnePointMove(() => List(l.head.newPredecessor).filter(_ >= v), () => myVRP.kFirst(3,myVRP.closestNeighboursForward,myVRP.isRouted),myVRP, hotRestart = false,selectDestinationBehavior = Best(),selectPointToMoveBehavior = First())),
    intermediaryStops = true,
    maxDepth = 3))


  val vlsnInsert = Mu[InsertPointMove](
  routeUnroutedPoint(3),
  l => if (myVRP.unroutedNodes.isEmpty) None else Some(routeUnroutedPoint(3)),
  intermediaryStops = false,
  maxDepth = 3)

  val remove = RemovePoint(() => myVRP.routed.value.filter(_>=v), myVRP,selectNodeBehavior = Best())
  def segExchange(k:Int) = SegmentExchange(myVRP,()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted),() => myVRP.vehicles)

  val swapInOut = Profile((remove andThen routeUnroutedPoint(10)) name ("SWAPInsert"))
  val doubleInsert = Profile((routeUnroutedPoint(10) andThen routeUnroutedPoint(10)) name ("doubleInsert"))
  val doubleRemove = Profile(( RemovePoint(() => myVRP.routed.value.filter(_>=v), myVRP,selectNodeBehavior = First())) andThen
    RemovePoint(() => myVRP.routed.value.filter(_>=v), myVRP,selectNodeBehavior = Best()) name ("doubleRemove"))

  val search = new RoundRobin(List(onePtMove(100),
    doubleInsert,
    doubleInsert,
    swapInOut,
    doubleRemove,
    swapInOut,
    doubleInsert,

    swapInOut,
    Profile(vlsnInsert),
    Profile(vlsnInsert andThen vlsn1pt),
    Profile(vlsnInsert andThen doubleRemove),
    Profile(vlsn1pt),
    threeOpt(5,false),
    twoOpt,
    segExchange(10))) onExhaustRestartAfter (doubleRemove acceptAll(),5,myVRP.obj)

  //search.verbose = 1
  search.verboseWithExtraInfo(2, ()=> "" + myVRP)

  print("Doing all moves ...")


  search.doAllMoves(obj = myVRP.obj)
  model.propagate()
  println(search.profilingStatistics)

  println(myVRP)
}
