package oscar.cbls.benchmarks

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
import oscar.cbls.lib.invariant.seq.Size
import oscar.cbls.modeling.Algebra._
import oscar.cbls.core.objective.Objective
import oscar.cbls.business.routing.model._
import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}
import oscar.cbls.util.StopWatch

import scala.util.Random

class MySimpleRoutingWithUnroutedPoints(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int)
  extends VRP(n,v,m,maxPivot)
  with TotalConstantDistance with ClosestNeighbors with RoutedAndUnrouted{

  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  val penaltyForUnrouted  = 10000

  val obj = Objective(totalDistance + (penaltyForUnrouted*(n - Size(routes))))

  override def toString : String = super.toString + "objective: " + obj.value + "\n"

  val closestNeighboursForward = computeClosestNeighborsForward()

  def size = routes.value.size
}

object SymmetricVRPBench extends App {

  val n = 10000
  val v = 100

  val verbose = 0
  val maxPivotPerValuePercent = 4
  new TSPRoutePointsS(1000,100,4,verbose)
  System.gc()

  val nbTrials = 10

  println()
  print("n\tv\tpercent")
  for (t <- 1 to nbTrials) {
    print("\ttime")
  }
  println


  for(n <- 11000 to 11000 by 2000){
    for(v <- List(100)){
      for (maxPivotPerValuePercent <- List(0,1,2,3,4,5,20)) {
        print(n + "\t" + v + "\t" + maxPivotPerValuePercent + "\t")
        for (t <- 1 to nbTrials){
          new TSPRoutePointsS(n, v, maxPivotPerValuePercent, verbose)
          print("\t")
          System.gc()
        }
        println
      }
    }
  }
}

class TSPRoutePointsS(n:Int,v:Int,maxPivotPerValuePercent:Int, verbose:Int) extends StopWatch{

  val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1

  startWatch()
  //  println("restrictions:" + restrictions)
  val model = new Store() //checker = Some(new ErrorChecker()))

  val myVRP = new MySimpleRoutingWithUnroutedPoints(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent)
  val nodes = myVRP.nodes

  model.close()

  val bestInsert = false

  val routeUnroutdPoint =  Profile(new InsertPointUnroutedFirst(myVRP.unrouted,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,best=bestInsert,neighborhoodName = "InsertUF"))

  //TODO: using post-filters on k-nearest is probably crap
  val routeUnroutdPoint2 =  Profile(new InsertPointRoutedFirst(myVRP.routed,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,x => !myVRP.isRouted(x)),myVRP,best=bestInsert,neighborhoodName = "InsertRF")  guard(() => myVRP.size < n/2))

  def onePtMove(k:Int) = Profile(new OnePointMove(myVRP.routed, () => myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP))

  val twoOpt = Profile(new TwoOpt1(myVRP.routed, ()=>myVRP.kFirst(20,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP))

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(myVRP.routed, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val search = (BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, onePtMove(10),twoOpt, threeOpt(10,true))) exhaust threeOpt(20,true))

  // val search = (new RoundRobin(List(routeUnroutdPoint2,onePtMove(10) guard (() => myVRP.unrouted.value.size != 0)),10)) exhaust BestSlopeFirst(List(onePtMove(20),twoOpt, threeOpt(10,true))) exhaust threeOpt(20,true)

  search.verbose = verbose
  //search.verboseWithExtraInfo(1, ()=> "" + myVRP)

  search.doAllMoves(obj=myVRP.obj)

  print(getWatch)
}

object RoutingMatrixGenerator {
  val random = new Random(0)

  def apply(N : Int, side : Int = 1000) : (Array[Array[Int]], Array[(Int, Int)]) = {

    //we generate te cost distance matrix
    def randomXY : Int = (random.nextFloat() * side).toInt
    val pointPosition : Array[(Int, Int)] = Array.tabulate(N)(w => (randomXY, randomXY))

    def distance(from : (Int, Int), to : (Int, Int)) =
      math.sqrt(math.pow(from._1 - to._1, 2) + math.pow(from._2 - to._2, 2)).toInt

    //for each delivery point, the distance to each warehouse
    (Array.tabulate(N)(
      n1 => Array.tabulate(N)(
        n2 => distance(pointPosition(n1), pointPosition(n2)))), pointPosition)
  }
}
