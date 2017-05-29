package oscar.cbls.test.invariants

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

import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import oscar.cbls.core.computation.{CBLSIntVar, IntValue}
import oscar.cbls.lib.constraint._
import oscar.cbls.lib.invariant.logic.{DenseCount, Elements, Filter, IntElement, IntITE, SelectLEHeapHeap, SetElement, _}
import oscar.cbls.lib.invariant.minmax.{ArgMax, ArgMin, Max2, MaxArray, MaxLin, MaxSet, Min2, MinArray, MinLin, MinSet}
import oscar.cbls.lib.invariant.numeric.{Abs, Div, Minus, Mod, Prod, Prod2, ProdElements, RoundUpModulo, Step, Sum, Sum2, SumElements}
import oscar.cbls.lib.invariant.routing._
import oscar.cbls.lib.invariant.routing.oldCapa.ForwardCumulativeConstraintOnVehicle
import oscar.cbls.lib.invariant.seq._
import oscar.cbls.lib.invariant.set.{Cardinality, Diff, Inter, Interval, MakeSet, SetProd, SetSum, TakeAny, Union, UnionAll}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.test.invariants.bench._
import oscar.cbls.test.routing.RoutingMatrixGenerator

import scala.collection.immutable.SortedMap

/**
 * @author yoann.guyot@cetic.be
 */
class InvariantTests extends FunSuite with Checkers {


  {
  var assertActivated = false
  assert({assertActivated = true; true})
  if(!assertActivated)
    println("You are executing tests with asserts deactivated, you should activate them for a more thorough test")
  }

  val verbose = 0

  test("BelongsTo maintains the violation of a membership.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    new BelongsTo(bench.genIntVar(0 to 10), bench.genIntSetVar(5, 0 to 10))
    bench.run
  }

  test("LE maintains the violation of a lesser or equal test.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    new LE(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("GE maintains the violation of a greater or equal test.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    new GE(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("L maintains the violation of a strict lesser test.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    new L(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("G maintains the violation of a strict greater test.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    new G(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("NE maintains the violation of a inequality test.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    new NE(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("EQ maintains the violation of an equality test.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    new EQ(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("AllDiff maintains output = all int vars have different values") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    new AllDiff(bench.genIntVarsArray(10, -10 to 10))
    bench.run
  }

  test("AtLeast") {
    def myMapValues[B,C](s:SortedMap[Int,B],f:B=>C):SortedMap[Int,C] =
      s.foldLeft[SortedMap[Int,C]](SortedMap.empty)((acc,couple) => acc+((couple._1,f(couple._2))))

    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    val constantMap = InvGen.randomIntSortedMap(10, 0 to 30, 0 to 30)
    new AtLeast(bench.genIntVars(10), myMapValues(constantMap, (_:Int) => bench.genIntVar(0 to 30)))
    bench.run
  }

  test("AtMost") {

    def myMapValues[B,C](s:SortedMap[Int,B],f:B=>C):SortedMap[Int,C] =
      s.foldLeft[SortedMap[Int,C]](SortedMap.empty)((acc,couple) => acc+((couple._1,f(couple._2))))

    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    val constantMap = InvGen.randomIntSortedMap(10, 0 to 30, 0 to 30)
    val atmost = new AtMost(bench.genIntVars(10, range = 0 to 30),myMapValues(constantMap, (_:Int) => bench.genIntVar(0 to 30)))

    bench.run
  }

  test("MultiKnapsack") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    new MultiKnapsack(
      bench.genIntVarsArray(10, 0 to 5),
      bench.genIntVarsArray(10, 2 to 7),
      bench.genIntVarsArray(6, 1 to 10))
    bench.run
  }

  test("Sequence") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    new Sequence(
      bench.genIntVarsArray(),
      Gen.choose(10, 20).sample.get,
      Gen.choose(2, 9).sample.get,
      Array.tabulate(101)(n => (n%3 == 0)))
    bench.run
  }

  test("Access to ITE maintains output = if ifVar > 0 then thenVar else elseVar") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    new IntITE(
      bench.genIntVar(-2 to 3),
      bench.genIntVar(1 to 2),
      bench.genIntVar(10 to 11))
    bench.run
  }

  test("Access to int element maintains output = array(index)") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    new IntElement(
      bench.genIntVar(0 to 19),
      bench.genIntVarsArray(20, 0 to 100))
    bench.run
  }

  test("Access to int vars...") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Elements(
      bench.genIntSetVar(3, 0 to 4),
      bench.genIntVarsArray(5, 0 to 10))
    bench.run
  }

  test("Access to int set element maintains output = array(index)") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new SetElement(
      bench.genIntVar(0 to 19),
      bench.genIntSetVars(20, 10, 0 to 100))
    bench.run
  }

  test("Sparse Cluster maintains a cluster of the indexes of an array.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Cluster.MakeSparse(bench.genIntVarsArray(50),
      (Gen.containerOfN[List, Int](100, Gen.choose(0, 100))).sample.get)
    bench.run
  }

  test("Dense Cluster maintains a cluster of all the indexes of an array.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Cluster.MakeDense(bench.genIntVarsArray(50))
    bench.run
  }

  test("Dense Cluster maintains a cluster of all the indexes of an array"
    + " (assuming min and max).") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Cluster.MakeDenseAssumingMinMax(bench.genIntVarsArray(50), 0, 100)
    bench.run
  }

  test("Dense Count maintains count(j) = #{i in index of values | values[i] == j}") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new DenseCount(
      bench.genIntVarsArray(10, 0 to 19),
      bench.genIntVarsArray(20, 0 to 19, false))
    bench.run
  }

  //ignore("Cross references...")(pending) it was tested extensively in the routing cases

  //ignore("Cumulative...")(pending) since it was tested in the scheduling examples.

  test("Filter...") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Filter(
      bench.genIntVarsArray(4, 0 to 5),
      (i: Int) => (i % 2) == 0)
    bench.run
  }

  test("SelectLEHeapHeap") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new SelectLEHeapHeap(
      bench.genIntVarsArray(4, 0 to 5),
      bench.genIntVar(3 to 10))
    bench.run
  }

  //ignore("SelectLESetQueue") {
  // the forbidden value chenges are not forbidden in this test, so it fails
  //  //le pivot ne peut qu'augmenter
    //une valeur en dessous du pivot ne peut que prendre une valeur dépassant toutes les autres valeurs
    //les valeurs au dessus du pivot ne peuvent pas changer
  //  val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
  //  new SelectLESetQueue(bench.genIntVarsArray(5, 0 to 5), bench.genIntVar(3 to 10, false)).toIntSetVar
  //  bench.run
  //}

  //ignore("Predecessor")(pending)

  //ignore("Routes")(pending)

  //this is not working so far.
  test("Sort") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Sort.MakeSort(bench.genIntVarsArray(4, 0 to 30))
    bench.run
  }

  // TODO test also with the other parameters of ArgMinArray
  test("ArgMinArray maintains the set of min variables of the array") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new ArgMin(bench.genIntVarsArray(20, 0 to 30))
    bench.run
  }

  // TODO test also with the other parameters of ArgMaxArray
  test("ArgMaxArray maintains the set of max variables of the array") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new ArgMax(bench.genIntVarsArray(20, 0 to 30))
    bench.run
  }

  test("MaxLin") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new MaxLin(bench.genSortedIntVars(6, -10 to 10))
    bench.run
  }

  test("MinLin") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new MinLin(bench.genSortedIntVars(6, 0 to 10))
    bench.run
  }
/*
  test("Min") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Min(bench.genSortedIntVars(5, -10 to 10))
    bench.run
  }
  
  test("Max") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Max(bench.genSortedIntVars(5, -10 to 10))
    bench.run
  }
*/
  test("Min2") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Min2(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("Max2") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Max2(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("MinArray maintains the minimum from an array of variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new MinArray(bench.genIntVarsArray(4, 0 to 100))
    bench.run
  }

  test("MaxArray maintains the maximum from an array of variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new MaxArray(bench.genIntVarsArray(2, 0 to 50))
    bench.run
  }

  test("MinSet maintains the minimum of a set.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new MinSet(bench.genIntSetVar(),Default=100)
    bench.run
  }

  test("MaxSet maintains the maximum of a set") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new MaxSet(bench.genIntSetVar(),Default=0)
    bench.run
  }

  test("SumElements maintains the sum of variables of which indices are in the given set.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new SumElements(
      bench.genIntVarsArray(10, 0 to 100),
      bench.genIntSetVar(5, 0 to 9))
    bench.run
  }

  test("ProdElements maintains the product of variables of which indices are in the given set.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new ProdElements(
      bench.genIntVarsArray(10, 0 to 10),
      bench.genIntSetVar(2, 0 to 9))
    bench.run
  }

  test("Sum maintains the sum of input variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Sum(bench.genIntVarsArray(10, 0 to 100))
    bench.run
  }

  test("Prod maintains the product of input variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Prod(bench.genIntVarsArray(3, 0 to 100))
    bench.run
  }

  test("Minus maintains the difference between two variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Minus(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100))
    bench.run
  }

  test("Sum2 maintains the sum of two variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Sum2(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100))
    bench.run
  }

  test("Prod2 maintains the product of two variables") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Prod2(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100))
    bench.run
  }

  test("Div maintains the division of two variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Div(bench.genIntVar(0 to 100),
      bench.genIntVar(1 to 100, true, (v: Int) => v != 0))
    bench.run
  }

  test("Mod maintains the modulo of two variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Mod(bench.genIntVar(0 to 100),
      bench.genIntVar(1 to 100, true, (v: Int) => v != 0))
    bench.run
  }

  test("Abs maintains the absolute value of a variable.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Abs(bench.genIntVar(-100 to 100))
    bench.run
  }

  test("Step maintains a step function of the input var.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Step(bench.genIntVar(-100 to 100))
    bench.run
  }

  test("RoundUpModulo") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new RoundUpModulo(
      bench.genIntVar(0 to 10),
      bench.genIntVar(0 to 5),
      Gen.choose(10, 20).sample.get,
      Gen.choose(1, 5).sample.get,
      Gen.choose(0, 10).sample.get)
    bench.run
  }

 /* test("RoundUpCustom") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new RoundUpCustom(
      bench.genIntVar(0 to 10),
      bench.genIntVar(1 to 10),
      InvGen.randomTuples(10, 0 to 10).map(ab => (ab._1,ab._1 + ab._2)))
    bench.run
  }*/

  test("Union maintains the union of two sets.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Union(bench.genIntSetVar(), bench.genIntSetVar())
    bench.run
  }
  
  test("UnionAll maintains the union of a set of sets.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new UnionAll(bench.genIntSetVars())
    bench.run
  }

  test("Inter maintains the intersection of two sets.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Inter(bench.genIntSetVar(), bench.genIntSetVar())
    bench.run
  }

  test("Diff maintains the difference between two sets.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Diff(bench.genIntSetVar(), bench.genIntSetVar())
    bench.run
  }

  test("Cardinality maintains the cardinality of a set.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Cardinality(bench.genIntSetVar())
    bench.run
  }

  test("MakeSet maintains an IntSetVar given a set of IntVar.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new MakeSet(bench.genSortedIntVars(10, 0 to 10))
    bench.run
  }

  test("Interval maintains the set in the interval.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new Interval(
      bench.genIntVar(-100 to 100),
      bench.genIntVar(-100 to 100))
    bench.run
  }

  test("TakeAny maintains a value taken from the set.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new TakeAny(bench.genIntSetVar(), 0)
    bench.run
  }

  test("SetSum maintains the sum of variables (after optionnaly appliying a function).") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new SetSum(bench.genIntSetVar())
    bench.run
  }

  /**
   * Won't pass when the product products an overflow.
   */
  test("SetProd maintains the product of variables") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new SetProd(bench.genIntSetVar(10, -3 to 3))
    bench.run
  }


/*  test("IdentityInt maintains the identity of an integer).") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new IdentityInt(bench.genIntVar(-100 to 100))
    bench.run
  }
*/

  /*
  test("IdentityIntSet maintains the identity of a set of integers).") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new IdentitySet(bench.genIntSetVar())
    bench.run
  }
  */


  test ("ConcatenateVars concatenates two ChangingSeqValue") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    //val seqVars = bench.genIntSeqVars(2)
    val seqVar1 = bench.genIntSeqVar()
    val seqVar2 = bench.genIntSeqVar()
    Size(new Concatenate(seqVar1,seqVar2,4,20))
    bench.run
  }


  test ("ConcatenateList Var concatenates List and ChangingSeqValue") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    //val seqVars = bench.genIntSeqVars(2)
    val seqVar2 = bench.genIntSeqVar()
    Size(new ConcatenateFirstConstant(List(1,6,8,30),seqVar2,4,20))
    bench.run
  }

  test ("PositionsOf maintains the positions of a value"){
    val bench = new InvBench(verbose,List(PlusOne(), Random(), Shuffle()))
    val seqVar = bench.genIntSeqVar()
    val value = seqVar.value.valueAtPosition(0).get
    PositionsOf(seqVar,value)
    bench.run
  }

  test ("Map "){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val seqVar = bench.genIntSeqVar(range = 0 to 100)
    oscar.cbls.lib.invariant.seq.Map(seqVar, Array.tabulate(101)(n => 2*n))
    bench.run
  }

  test ("Precedence"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val seqVar = bench.genNotRandomIntSeqVar(100)
    val precedences = RoutingMatrixGenerator.generatePrecedence(seqVar.value.size,0,seqVar.value.size/2)
    Precedence(seqVar,precedences)
    bench.run()
  }

  test ("Successors"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val seqVar = bench.genIntSeqVar(25)
    Successors(seqVar)
    bench.run()
  }

  test ("Size maintains the size of the sequence"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val seqVar = bench.genIntSeqVar(25)
    new Size(seqVar)
    bench.run()
  }

  test ("OccurenceOf maintains the occurence of a certain value"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val seqVar = bench.genIntSeqVar()
    new OccurrencesOf(seqVar,Gen.choose(0, 100).sample.get)
    bench.run()
  }

  test("Content maintains the content of the intSeq"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val seqVar = bench.genIntSeqVar()
    Content(seqVar)
    bench.run()
  }

  /*test("Flip maintains a flipped sequence"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(),Shuffle()))
    val seqVar = bench.genIntSeqVar()
    Flip(seqVar)
    bench.run()
  }*/

  test("SeqSum maintains the sum of all values in a sequence"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(),Shuffle()))
    val seqVar = bench.genIntSeqVar()
    SeqSum(seqVar)
    bench.run()
  }

  // ---- Routing tests ---- //
  test("Node of vehicle"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(),Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n,v)
    NodeOfVehicle(route,v)
    bench.run()
  }

  test("Constant routing distance - global distance - symmetric"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(),Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n-1,v)
    val distanceMatrix = RoutingMatrixGenerator(n)._1
    ConstantRoutingDistance(route,n,v,false,distanceMatrix,true)
    bench.run()
  }

  test("Constant routing distance - per vehicle distance - symetric"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(),Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n-1,v)
    val distanceMatrix = RoutingMatrixGenerator(n)._1
    ConstantRoutingDistance(route,n,v,true,distanceMatrix,true)
    bench.run()
  }

  test("NodeVehicleRestrictions"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(),Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n,v)
    val nodeVehicleRestrictions = RoutingMatrixGenerator.generateRestrictions(n,v,(n/v).toInt)
    NodeVehicleRestrictions(route,v,nodeVehicleRestrictions)
    bench.run()
  }

  test("RouteSuccessorsAndPredecessors"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(), Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n,v)
    val defaultWhenNotInSeq = -1
    RouteSuccessorAndPredecessors(route,v,defaultWhenNotInSeq)
    bench.run()
  }

  test("VehicleOfNodes"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(), Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n,v)
    VehicleOfNodes(route,v)
    bench.run()
  }


  test("GenericCumulativeIntegerDimensionOnVehicle"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(), Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n,v)

    val limite = 10
    def genMatrix(node:Int):Array[Array[Int]] = {
      val init = Int.MinValue
      val matrix: Array[Array[Int]] = Array.tabulate(n,n)((n1:Int,n2:Int)=>init)
      for (elt <- 0 until node) {
        for (elt1 <- 0 until node) {
          matrix(elt)(elt1) =  if(elt==elt1) 0 else scala.util.Random.nextInt(limite)
        }
      }
      matrix
    }
    val matrix = genMatrix(n)
    def genOperation(node:Int):Array[Array[Int]] = {
      val oper: Array[Array[Int]] = Array.ofDim(n,n)
      var t1:Array[Int] =Array.ofDim(n)
      for (elt <- 0 until node) {
        for (elt1 <- 0 until node) {
          oper(elt)(elt1) =  if(matrix(elt)(elt1)==0) scala.util.Random.nextInt(3) else scala.util.Random.nextInt(4)
        }
      }
      oper
    }

    val oper = genOperation(n)
    def op(n1:Int,n2:Int,c:Int): Int= {
      oper(n1)(n2) match {
        case 0 => c + matrix(n1)(n2)
        case 1 => c - matrix(n1)(n2)
        case 2 => c * matrix(n1)(n2)
        case 3 => c % matrix(n1)(n2)
      }
    }

    def start() : Array[IntValue]= { Array.tabulate(v)((car:Int)=> scala.util.Random.nextInt(limite))}
    val  s = start()
    val inv = ForwardCumulativeIntegerDimensionOnVehicle(route,n,v,op,s,-1)

    bench.run()
  }

  test("GenericCumulativeIntegerDimensionOnVehicleWithVar"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(), Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n,v)
    val contentAtStart = bench.genIntVarsArray(v)

    val limite = 10
    def genMatrix(node:Int):Array[Array[Int]] = {
      val init = Int.MinValue
      val matrix: Array[Array[Int]] = Array.tabulate(n,n)((n1:Int,n2:Int)=>init)
      for (elt <- 0 until node) {
        for (elt1 <- 0 until node) {
          matrix(elt)(elt1) =  if(elt==elt1) 0 else scala.util.Random.nextInt(limite)
        }
      }
      matrix
    }
    val matrix = genMatrix(n)
    def genOperation(node:Int):Array[Array[Int]] = {
      val oper: Array[Array[Int]] = Array.ofDim(n,n)
      var t1:Array[Int] =Array.ofDim(n)
      for (elt <- 0 until node) {
        for (elt1 <- 0 until node) {
          oper(elt)(elt1) =  if(matrix(elt)(elt1)==0) scala.util.Random.nextInt(3) else scala.util.Random.nextInt(4)
        }
      }
      oper
    }

    val oper = genOperation(n)
    def op(n1:Int,n2:Int,c:Int): Int= {
      oper(n1)(n2) match {
        case 0 => c + matrix(n1)(n2)
        case 1 => c - matrix(n1)(n2)
        case 2 => c * matrix(n1)(n2)
        case 3 => c % matrix(n1)(n2)
      }
    }

    def start() : Array[CBLSIntVar]= { Array.tabulate(v)((car:Int)=> CBLSIntVar(route.model,op(car,car,0)))}
    val  s = start()

    var inv = ForwardCumulativeIntegerDimensionOnVehicle(route,n,v,op,contentAtStart,defaultForUnroutedNodes= -1,maxContent = 6)

    val go = System.nanoTime()
    bench.run()
    print("GenericCumulativeIntegerDimensionOnVehicleWithVar(n ="+n+" v ="+v+") : "+((System.nanoTime()-go)/Math.pow(10,9))+" s")
  }

  test("GenericCumulativeConstraint"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(), Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n,v)


    val limite = 10
    def genMatrix(node:Int):Array[Array[Int]] = {
      val init = Int.MinValue
      val matrix: Array[Array[Int]] = Array.tabulate(n,n)((n1:Int,n2:Int)=>init)
      for (elt <- 0 until node) {
        for (elt1 <- 0 until node) {
          matrix(elt)(elt1) =  if(elt==elt1) 0 else scala.util.Random.nextInt(limite)
        }
      }
      matrix
    }
    val matrix = genMatrix(n)
    def genOperation(node:Int):Array[Array[Int]] = {
      val oper: Array[Array[Int]] = Array.ofDim(n, n)
      var t1:Array[Int] =Array.ofDim(n)
      for (elt <- 0 until node) {
        for (elt1 <- 0 until node) {
          oper(elt)(elt1) =  if(matrix(elt)(elt1)==0) scala.util.Random.nextInt(3) else scala.util.Random.nextInt(4)
        }
      }
      oper
    }

    val oper = genOperation(n)
    def op(n1:Int,n2:Int,c:Int): Int= {
      oper(n1)(n2) match {
        case 0 => c + matrix(n1)(n2)
        case 1 => c - matrix(n1)(n2)
        case 2 => c * matrix(n1)(n2)
        case 3 => c % matrix(n1)(n2)
      }
    }

    def start() : Array[Int]= { Array.tabulate(v)((car:Int)=> scala.util.Random.nextInt(limite))}
    val  s = start()

    val inv = ForwardCumulativeConstraintOnVehicle(route,n,v,op,limite,s,
      maxCheckpointLevel = 2,
      maxStack = 1,
      capacityName = "test capacity"
    )


    val go = System.nanoTime()
    bench.run()
    println("GenericCumulativeConstraint(n ="+n+" v ="+v+") : "+((System.nanoTime()-go)/Math.pow(10,9)) + "s")
  }

  // ---- checkpoint Tests ---- //

  test("Star exploration"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), Shuffle(), MultipleMove()))
    val n = 100
    val v = 4
    val route = bench.genRouteOfNodesForCheckPoint(n-1,v)
    val distanceMatrix = RoutingMatrixGenerator(n)._1
    ConstantRoutingDistance(route,n,v,true,distanceMatrix,true)
    bench.run()
  }
}

