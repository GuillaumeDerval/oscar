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
import oscar.cbls.lib.invariant.seq.Precedence
import oscar.cbls.core.objective.{Objective, CascadingObjective, IntVarObjective}
import oscar.cbls.business.routing.model._
import oscar.cbls.business.routing.neighborhood.{OnePointMoveMove, OnePointMove, ThreeOpt, TwoOpt1}
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

import scala.collection.immutable.SortedSet


class MySimpleRoutingP(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int, precedences:List[(Int,Int)])
  extends VRP(n,v,m,maxPivot)
  with TotalConstantDistance with ClosestNeighbors with NextAndPrev{

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  //initializes to something simple; vehicle v-1 does all nodes (but other vehicles)
  setCircuit(nodes)

  setSymmetricDistanceMatrix(symmetricDistance)

  val precedenceInvar = Precedence(routes,precedences)

  val nodesStartingAPrecedence:SortedSet[Int] = precedenceInvar.nodesStartingAPrecedence
  val nodesEndingAPrecedenceStartedAt:(Int => Iterable[Int]) = precedenceInvar.nodesEndingAPrecedenceStartedAt

  val obj = new CascadingObjective(new IntVarObjective(precedenceInvar), totalDistance)
  //val obj = Objective(totalDistance)

  this.addToStringInfo(() => "precedences: " + precedences)
  this.addToStringInfo(() => "objective: " + obj)
  this.addToStringInfo(() => "n:" + n + " v:" + v)

  val nearestForward:Array[Iterable[Int]] = computeClosestNeighborsForward()
}

object RoutingWithPrecedence extends App{

  val n = 100
  val v = 1
  val nbPRecedences = (n - v) / 2

  val maxPivotPerValuePercent = 4

  println("VRPPrecedence(n:" + n + " v:" + v + ")")

  val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1

  val precedences = RoutingMatrixGenerator.generatePrecedence(n,v,nbPRecedences)

  //  println("restrictions:" + restrictions)
  val model = new Store(checker = Some(new ErrorChecker())) //)) //checker = Some() //new ErrorChecker()))

  val myVRP = new MySimpleRoutingP(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent,precedences)
  val nodes = myVRP.nodes

  model.close()

  println(myVRP)

  def onePtMove = Profile(OnePointMove(() => nodes, ()=>myVRP.kFirst(10,myVRP.nearestForward), myVRP))

  val twoPointMove = (OnePointMove(() => nodes, ()=>myVRP.kFirst(40,myVRP.nearestForward), myVRP,"firstPointMove") andThen OnePointMove(() => nodes, ()=>myVRP.kFirst(40,myVRP.nearestForward), myVRP,"secondPointMove")) name("TwoPointMove")
  val twoPointMoveSmart = Profile((OnePointMove(() => myVRP.nodesStartingAPrecedence, ()=>myVRP.kFirst(40,myVRP.nearestForward), myVRP,"firstPointMove",best=true) dynAndThen ((o:OnePointMoveMove) => {
    OnePointMove(() => myVRP.nodesEndingAPrecedenceStartedAt(o.movedPoint), ()=>myVRP.kFirst(40,myVRP.nearestForward), myVRP,"secondPointMove",best=true)})) name("SmartTwoPtMove"))

  val twoOpt = Profile(new TwoOpt1(() => nodes, ()=>myVRP.kFirst(40,myVRP.nearestForward), myVRP))

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(() => nodes, ()=>myVRP.kFirst(k,myVRP.nearestForward), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  //,,(onePtMove andThen onePtMove) name ("twoPointMove")
  val search = (
    new BestSlopeFirst(List(threeOpt(10,true),onePtMove,twoOpt,twoPointMoveSmart),refresh = 10)
      exhaust new BestSlopeFirst(List(threeOpt(20,true))))

  //val search = threeOpt(20,true)
  //search.verboseWithExtraInfo(2, ()=> "" + myVRP)
  search.verbose = 1

  search.doAllMoves(obj=myVRP.obj)

  println("final propagation: ")
  model.propagate()
  println("done.")

  println(model.getPropagationElements.toList)
  model.propagate()

  println
  println(myVRP)
  println
  println(search.profilingStatistics)
}
