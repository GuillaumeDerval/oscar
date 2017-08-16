/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.examples.cbls.routing

import oscar.cbls.core.computation.Store
import oscar.cbls.core.search.Move
import oscar.cbls.lib.invariant.seq.Size
import oscar.cbls.core.objective.Objective
import oscar.cbls.business.routing.model.{ClosestNeighbors, ConstantDistancePerVehicle, RoutedAndUnrouted, VRP}
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile, RoundRobin}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.util.StopWatch


class DemoRoutingController extends StopWatch{

  var customersNumber = 0
  var carsNumber = 0
  var myVRP:DrcVRP = _
  var model:Store = _

  def initiateProblem(customers:Int, cars:Int, s:Int,u:Int):List[(Double,Double)]={
    val generatedMatrix = RoutingMatrixGenerator(customers,s)
    customersNumber = customers
    carsNumber = cars
    model = new Store()
    myVRP = new DrcVRP(customersNumber, carsNumber, model, generatedMatrix._2.map(p => (p._1.toDouble,p._2.toDouble)), generatedMatrix._1, 4)
    model.close()
    Array.tabulate(customers)(n => (generatedMatrix._2(n)._1.toDouble, generatedMatrix._2(n)._2.toDouble)).toList
  }

  def resetProblem():Unit = {
    customersNumber = 0
    carsNumber = 0
    myVRP = null
    model = null
  }

  def resolveProblem:Boolean = {
    if(myVRP == null) false
    println("closed model " + getWatch + "ms")
    startWatch()

    val insertPointUnroutedFirst = Profile(InsertPointUnroutedFirst(
      unroutedNodesToInsert= () => myVRP.unroutedNodes,
      relevantPredecessor = () => myVRP.kFirst(10,myVRP.closestNeighboursForward, myVRP.isRouted),
      vrp = myVRP))

     val insertPointUnroutedFirstBest = Profile(InsertPointUnroutedFirst(
       unroutedNodesToInsert= () => myVRP.unroutedNodes,
       relevantPredecessor = () => myVRP.kFirst(10,myVRP.closestNeighboursForward, myVRP.isRouted),
       neighborhoodName = "insertPointUnroutedFirstBest",
       vrp = myVRP, best = true))

     val pivot = myVRP.n/2

     val onePointMove = Profile(OnePointMove(
       nodesToMove = myVRP.routed,
       relevantNewPredecessors = () => myVRP.kFirst(50,myVRP.closestNeighboursForward, myVRP.isRouted),
       vrp = myVRP))

     val twoOpt = Profile(TwoOpt1(
       segmentStartValues = myVRP.routed,
       relevantNewSuccessors = () => myVRP.kFirst(20,myVRP.closestNeighboursForward, myVRP.isRouted),
       vrp = myVRP))

     val threeOpt = Profile(ThreeOpt(
       potentialInsertionPoints = myVRP.routed,
       relevantNeighbors = () => myVRP.kFirst(20,myVRP.closestNeighboursForward, myVRP.isRouted),
       vrp = myVRP,
       skipOnePointMove = true))

     val segExchange = Profile(SegmentExchange(vrp = myVRP,
       relevantNeighbors = () => myVRP.kFirst(40,myVRP.closestNeighboursForward, myVRP.isRouted),
       vehicles=() => myVRP.vehicles.toList))

     val search = new RoundRobin(scala.collection.immutable.List(insertPointUnroutedFirst,onePointMove),10).afterMoveOnMove((m:Move) =>{
       val routesList:List[List[Int]] = (for(c <- 0 to carsNumber-1)yield myVRP.getRouteOfVehicle(c)).toList
       DemoRoutingView.drawMove(routesList,(myVRP.obj.newValue,getWatch, m.neighborhoodName), myVRP.distancePerVehicle)
     }) exhaust
       (new BestSlopeFirst(List(onePointMove,threeOpt,segExchange),refresh = customersNumber/2)).afterMoveOnMove((m:Move) =>{
         val routesList:List[List[Int]] = (for(c <- 0 to carsNumber-1)yield myVRP.getRouteOfVehicle(c)).toList
         DemoRoutingView.drawMove(routesList,(myVRP.obj.newValue,getWatch, m.neighborhoodName), myVRP.distancePerVehicle)
       }) // exhaust onePointMove exhaust segExchange//threeOpt //(new BestSlopeFirst(List(onePointMove,twoOpt,threeOpt)))

     search.verbose = 1
     //    search.verboseWithExtraInfo(3,() => myVRP.toString)
     //segExchange.verbose = 3
     search.doAllMoves(_ > 10*customersNumber, myVRP.obj)

     println("total time " + getWatch + "ms or  " + getWatchString)

     println("\nresult:\n" + myVRP)

     println(search.profilingStatistics)
     DemoRoutingView.displayEndStatistics(Profile.statisticsHeader,search.collectProfilingStatistics)

     true
   }
}

class DrcVRP(n:Int, v:Int, m:Store, pointsList:Array[(Double,Double)],
             symmetricDistanceMatrix:Array[Array[Int]],maxPivot:Int)
  extends VRP(n,v,m,maxPivot) with ConstantDistancePerVehicle
    with ClosestNeighbors
    with RoutedAndUnrouted
{
  setSymmetricDistanceMatrix(symmetricDistanceMatrix)
  override protected def getDistance(from:Int,to:Int) : Int = symmetricDistanceMatrix(from)(to)

  val penaltyForUnrouted = 100000

  val obj = Objective(totalDistance + (penaltyForUnrouted*(n - Size(routes))))

  override def toString : String = super.toString + "objective: " + obj.value + "\n"

  val closestNeighboursForward = computeClosestNeighborsForward()
}
