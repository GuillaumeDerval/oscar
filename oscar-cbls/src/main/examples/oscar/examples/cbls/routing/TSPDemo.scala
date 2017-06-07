package oscar.examples.cbls.routing

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

import oscar.cbls.business.routing.model._
import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.core.computation.{CBLSSetConst, Store}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.invariant.routing.ConstantRoutingDistance
import oscar.cbls.lib.invariant.seq.{Content, Size}
import oscar.cbls.lib.invariant.set.Diff
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Mu, Profile}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.util.StopWatch

import scala.collection.immutable.SortedSet


class MySimpleDemoWithUnroutedPoints2(n:Int,v:Int,symmetricDistance:Array[Array[Int]],pointsPositions:Array[(Int,Int)],m:Store, maxPivot:Int)
  extends VRP(n,v,m,maxPivot)
    with ClosestNeighbors
    with RoutingMapDisplay
{

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  val penaltyForUnrouted  = 10000

  val routed = Content(routes.createClone(50)).setName("routed nodes")
  val unrouted = Diff(CBLSSetConst(SortedSet(nodes:_*)),routed).setName("unrouted nodes")

  m.registerForPartialPropagation(unrouted)
  m.registerForPartialPropagation(routed)

  val totalDistance = ConstantRoutingDistance(routes, n, v ,false, symmetricDistance, true)(0)

  val obj = Objective(totalDistance + (penaltyForUnrouted*(n - Size(routes))))

  this.addToStringInfo(() => "objective: " + obj.value)
  this.addToStringInfo(() => "n:" + n + " v:" + v)
  this.initializeRoutingMap(pointsPositions.map(pp => (pp._1.toDouble,pp._2.toDouble)))

  val closestNeighboursForward = computeClosestNeighborsForward()

  def size = routes.value.size
}

object TSPDemo extends App {

  val n = 10000
  val v = 10
  val displayDelay = 1000 //ms
  val verbose = 1
  val maxPivotPerValuePercent = 3

  new TSPDemo(n,v,maxPivotPerValuePercent,verbose,displayDelay)
}

class TSPDemo(n:Int,v:Int,maxPivotPerValuePercent:Int, verbose:Int, displayDelay:Int) extends StopWatch{

  val routingMatrix = RoutingMatrixGenerator(n,side=1000)
  val symmetricDistanceMatrix = routingMatrix._1
  val pointsPositions = routingMatrix._2

  startWatch()
  val model = new Store()

  val myVRP = new MySimpleDemoWithUnroutedPoints2(n,v,symmetricDistanceMatrix,pointsPositions,model,maxPivotPerValuePercent)
  val nodes = myVRP.nodes

  model.close()

  val bestInsert = false

  val routeUnroutdPoint =  Profile(InsertPointUnroutedFirst(myVRP.unrouted,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,best=bestInsert,neighborhoodName = "InsertUF"))

  //TODO: using post-filters on k-nearest is probably a bit slower than possible in this context
  val routeUnroutdPoint2 =  Profile(InsertPointRoutedFirst(myVRP.routed,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,x => !myVRP.isRouted(x)),myVRP,best=bestInsert,neighborhoodName = "InsertRF")  guard(() => myVRP.size < n/2))

  def onePtMove(k:Int) = Profile(OnePointMove(myVRP.routed, () => myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP))

  val twoOpt = Profile(TwoOpt1(myVRP.routed, ()=>myVRP.kFirst(20,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP))

  def threeOpt(k:Int, breakSym:Boolean) = Profile(ThreeOpt(myVRP.routed, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val vlsn1pt = Mu[OnePointMoveMove](
    OnePointMove(myVRP.routed, () => myVRP.kFirst(5,myVRP.closestNeighboursForward,myVRP.isRouted),myVRP),
    l => Some(OnePointMove(() => List(l.head.newPredecessor).filter(_ >= v), () => myVRP.kFirst(3,myVRP.closestNeighboursForward,myVRP.isRouted),myVRP, hotRestart = false)),
    intermediaryStops = true,
    maxDepth = 6)

  def segExchange(k:Int) = SegmentExchange(myVRP,()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted),() => myVRP.vehicles)
  var lastDisplay = this.getWatch

  val search = (BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, vlsn1pt, onePtMove(10),twoOpt, threeOpt(10,true),segExchange(10))) exhaust BestSlopeFirst(List(threeOpt(30,true),vlsn1pt))).afterMove(
    if(this.getWatch > lastDisplay + displayDelay) {myVRP.drawRoutes(); lastDisplay = this.getWatch})

  search.verbose = verbose
  //search.verboseWithExtraInfo(1, ()=> "" + myVRP)

  search.doAllMoves(obj=myVRP.obj)

  myVRP.drawRoutes()
  print(myVRP)
}
