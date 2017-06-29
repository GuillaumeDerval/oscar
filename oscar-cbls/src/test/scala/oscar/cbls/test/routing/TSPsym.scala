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
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.invariant.routing.NodeVehicleRestrictions
import oscar.cbls.lib.invariant.seq.{PositionsOf, Size}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.core.objective.{CascadingObjective, Objective}
import oscar.cbls.business.routing.model._
import oscar.cbls.business.routing.neighborhood.{OnePointMove, ThreeOpt, TwoOpt1}
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

import scala.util.Random


class MySimpleRouting(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int)
  extends VRP(n,v,m,maxPivot)
  with TotalConstantDistance with ClosestNeighbors {

  //initializes to something simple; vehicle v-1 does all nodes (but other vehicles)
  //initialization must be done ASAP, to encure that invariants will initialize straight based on this value
  setCircuit(nodes)

  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  //val obj = new CascadingObjective(totalViolationOnRestriction, totalDistance)
  val obj = Objective(totalDistance)

  override def toString : String = super.toString + "objective: " + obj.value + "\n"

  val closestNeighboursForward = computeClosestNeighborsForward()
}

object TSPsym extends App{

  val n = 1000
  val v = 1

  val maxPivotPerValuePercent = 4

  println("VRP(n:" + n + " v:" + v + ")")

  val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1

  //  println("restrictions:" + restrictions)
  val model = new Store(checker = Some(new ErrorChecker()))

  val myVRP = new MySimpleRouting(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent)
  val nodes = myVRP.nodes

  model.close()

  println(myVRP)

  val onePtMove = Profile(new OnePointMove(() => nodes, ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward), myVRP))

  val twoOpt = Profile(new TwoOpt1(() => nodes, ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward), myVRP))

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(() => nodes, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  //val search = BestSlopeFirst(List(onePtMove,twoOpt, threeOpt(10,true))) exhaust threeOpt(20,true)

  val search = threeOpt(10,true)

  search.verbose = 1

  search.doAllMoves(obj=myVRP.obj)

  model.propagate()

  println
  println(myVRP)
  println
  println(search.profilingStatistics)
}
