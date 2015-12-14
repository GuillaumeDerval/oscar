package oscar.cbls.test.invariants

import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import oscar.cbls.constraints.lib.basic.{BelongsTo, EQ, G, GE, L, LE, NE}
import oscar.cbls.constraints.lib.global.{AllDiff, AtLeast, AtMost, MultiKnapsack, Sequence}
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.logic.{DenseCount, Elements, Filter, IntElement, IntITE, SelectLEHeapHeap, SetElement, _}
import oscar.cbls.invariants.lib.minmax.{ArgMax, ArgMin, Max2, MaxArray, MaxLin, MaxSet, Min2, MinArray, MinLin, MinSet}
import oscar.cbls.invariants.lib.numeric.{Abs, Div, Minus, Mod, Prod, Prod2, ProdElements, RoundUpModulo, Step, Sum, Sum2, SumElements}
import oscar.cbls.invariants.lib.set.{Cardinality, Diff, Inter, Interval, MakeSet, SetProd, SetSum, TakeAny, Union}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.test.invariants.bench.{InvBench, InvGen}
import scala.collection.immutable.SortedMap
import oscar.cbls.invariants.lib.set.UnionAll

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
    val bench = new InvBench(verbose)
    new BelongsTo(bench.genIntVar(0 to 10), bench.genIntSetVar(5, 0 to 10))
    bench.run
  }

  test("LE maintains the violation of a lesser or equal test.") {
    val bench = new InvBench(verbose)
    new LE(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("GE maintains the violation of a greater or equal test.") {
    val bench = new InvBench(verbose)
    new GE(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("L maintains the violation of a strict lesser test.") {
    val bench = new InvBench(verbose)
    new L(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("G maintains the violation of a strict greater test.") {
    val bench = new InvBench(verbose)
    new G(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("NE maintains the violation of a inequality test.") {
    val bench = new InvBench(verbose)
    new NE(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("EQ maintains the violation of an equality test.") {
    val bench = new InvBench(verbose)
    new EQ(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("AllDiff maintains output = all int vars have different values") {
    val bench = new InvBench(verbose)
    new AllDiff(bench.genIntVarsArray(10, -10 to 10))
    bench.run
  }

  test("AtLeast") {
    def myMapValues[B,C](s:SortedMap[Int,B],f:B=>C):SortedMap[Int,C] =
      s.foldLeft[SortedMap[Int,C]](SortedMap.empty)((acc,couple) => acc+((couple._1,f(couple._2))))

    val bench = new InvBench(verbose)
    val constantMap = InvGen.randomIntSortedMap(10, 0 to 30, 0 to 30)
    new AtLeast(bench.genIntVars(10), myMapValues(constantMap, (_:Int) => bench.genIntVar(0 to 30)))
    bench.run
  }

  test("AtMost") {

    def myMapValues[B,C](s:SortedMap[Int,B],f:B=>C):SortedMap[Int,C] =
      s.foldLeft[SortedMap[Int,C]](SortedMap.empty)((acc,couple) => acc+((couple._1,f(couple._2))))

    val bench = new InvBench(verbose)
    val constantMap = InvGen.randomIntSortedMap(10, 0 to 30, 0 to 30)
    val atmost = new AtMost(bench.genIntVars(10, range = 0 to 30),myMapValues(constantMap, (_:Int) => bench.genIntVar(0 to 30)))

    bench.run
  }

  test("MultiKnapsack") {
    val bench = new InvBench(verbose)
    new MultiKnapsack(
      bench.genIntVarsArray(10, 0 to 5),
      bench.genIntVarsArray(10, 2 to 7),
      bench.genIntVarsArray(6, 1 to 10))
    bench.run
  }

  test("Sequence") {
    val bench = new InvBench(verbose)
    new Sequence(
      bench.genIntVarsArray(),
      Gen.choose(10, 20).sample.get,
      Gen.choose(2, 9).sample.get,
      (x: Int) => x > 1)
    bench.run
  }

  test("Access to ITE maintains output = if ifVar > 0 then thenVar else elseVar") {
    val bench = new InvBench(verbose)
    new IntITE(
      bench.genIntVar(-2 to 3),
      bench.genIntVar(1 to 2),
      bench.genIntVar(10 to 11))
    bench.run
  }

  test("Access to int element maintains output = array(index)") {
    val bench = new InvBench(verbose)
    new IntElement(
      bench.genIntVar(0 to 19),
      bench.genIntVarsArray(20, 0 to 100))
    bench.run
  }

  test("Access to int vars...") {
    val bench = new InvBench(verbose)
    new Elements(
      bench.genIntSetVar(3, 0 to 4),
      bench.genIntVarsArray(5, 0 to 10))
    bench.run
  }

  test("Access to int set element maintains output = array(index)") {
    val bench = new InvBench(verbose)
    new SetElement(
      bench.genIntVar(0 to 19),
      bench.genIntSetVars(20, 10, 0 to 100))
    bench.run
  }

  test("Sparse Cluster maintains a cluster of the indexes of an array.") {
    val bench = new InvBench(verbose)
    Cluster.MakeSparse(bench.genIntVarsArray(50),
      (Gen.containerOfN[List, Int](100, Gen.choose(0, 100))).sample.get)
    bench.run
  }

  test("Dense Cluster maintains a cluster of all the indexes of an array.") {
    val bench = new InvBench(verbose)
    Cluster.MakeDense(bench.genIntVarsArray(50))
    bench.run
  }

  test("Dense Cluster maintains a cluster of all the indexes of an array"
    + " (assuming min and max).") {
    val bench = new InvBench(verbose)
    Cluster.MakeDenseAssumingMinMax(bench.genIntVarsArray(50), 0, 100)
    bench.run
  }

  test("Dense Count maintains count(j) = #{i in index of values | values[i] == j}") {
    val bench = new InvBench(verbose)
    new DenseCount(
      bench.genIntVarsArray(10, 0 to 19),
      bench.genIntVarsArray(20, 0 to 19, false))
    bench.run
  }

  //ignore("Cross references...")(pending) it was tested extensively in the routing cases

  //ignore("Cumulative...")(pending) since it was tested in the scheduling examples.

  test("Filter...") {
    val bench = new InvBench(verbose)
    new Filter(
      bench.genIntVarsArray(4, 0 to 5),
      (i: Int) => (i % 2) == 0)
    bench.run
  }

  test("SelectLEHeapHeap") {
    val bench = new InvBench(verbose)
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
  //  val bench = new InvBench(verbose)
  //  new SelectLESetQueue(bench.genIntVarsArray(5, 0 to 5), bench.genIntVar(3 to 10, false)).toIntSetVar
  //  bench.run
  //}

  //ignore("Predecessor")(pending)

  //ignore("Routes")(pending)

  //this is not working so far.
  test("Sort") {
    val bench = new InvBench(verbose)
    Sort.MakeSort(bench.genIntVarsArray(4, 0 to 30))
    bench.run
  }

  // TODO test also with the other parameters of ArgMinArray
  test("ArgMinArray maintains the set of min variables of the array") {
    val bench = new InvBench(verbose)
    new ArgMin(bench.genIntVarsArray(20, 0 to 30))
    bench.run
  }

  // TODO test also with the other parameters of ArgMaxArray
  test("ArgMaxArray maintains the set of max variables of the array") {
    val bench = new InvBench(verbose)
    new ArgMax(bench.genIntVarsArray(20, 0 to 30))
    bench.run
  }

  test("MaxLin") {
    val bench = new InvBench(verbose)
    new MaxLin(bench.genSortedIntVars(6, -10 to 10))
    bench.run
  }

  test("MinLin") {
    val bench = new InvBench(verbose)
    new MinLin(bench.genSortedIntVars(6, 0 to 10))
    bench.run
  }
/*
  test("Min") {
    val bench = new InvBench(verbose)
    new Min(bench.genSortedIntVars(5, -10 to 10))
    bench.run
  }
  
  test("Max") {
    val bench = new InvBench(verbose)
    new Max(bench.genSortedIntVars(5, -10 to 10))
    bench.run
  }
*/
  test("Min2") {
    val bench = new InvBench(verbose)
    new Min2(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("Max2") {
    val bench = new InvBench(verbose)
    new Max2(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run
  }

  test("MinArray maintains the minimum from an array of variables.") {
    val bench = new InvBench(verbose)
    new MinArray(bench.genIntVarsArray(4, 0 to 100))
    bench.run
  }

  test("MaxArray maintains the maximum from an array of variables.") {
    val bench = new InvBench(verbose)
    new MaxArray(bench.genIntVarsArray(2, 0 to 50))
    bench.run
  }

  test("MinSet maintains the minimum of a set.") {
    val bench = new InvBench(verbose)
    new MinSet(bench.genIntSetVar(),Default=100)
    bench.run
  }

  test("MaxSet maintains the maximum of a set") {
    val bench = new InvBench(verbose)
    new MaxSet(bench.genIntSetVar(),Default=0)
    bench.run
  }

  test("SumElements maintains the sum of variables of which indices are in the given set.") {
    val bench = new InvBench(verbose)
    new SumElements(
      bench.genIntVarsArray(10, 0 to 100),
      bench.genIntSetVar(5, 0 to 9))
    bench.run
  }

  test("ProdElements maintains the product of variables of which indices are in the given set.") {
    val bench = new InvBench(verbose)
    new ProdElements(
      bench.genIntVarsArray(10, 0 to 10),
      bench.genIntSetVar(2, 0 to 9))
    bench.run
  }

  test("Sum maintains the sum of input variables.") {
    val bench = new InvBench(verbose)
    new Sum(bench.genIntVarsArray(10, 0 to 100))
    bench.run
  }

  test("Prod maintains the product of input variables.") {
    val bench = new InvBench(verbose)
    new Prod(bench.genIntVarsArray(3, 0 to 100))
    bench.run
  }

  test("Minus maintains the difference between two variables.") {
    val bench = new InvBench(verbose)
    new Minus(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100))
    bench.run
  }

  test("Sum2 maintains the sum of two variables.") {
    val bench = new InvBench(verbose)
    new Sum2(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100))
    bench.run
  }

  test("Prod2 maintains the product of two variables") {
    val bench = new InvBench(verbose)
    new Prod2(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100))
    bench.run
  }

  test("Div maintains the division of two variables.") {
    val bench = new InvBench(verbose)
    new Div(bench.genIntVar(0 to 100),
      bench.genIntVar(1 to 100, true, (v: Int) => v != 0))
    bench.run
  }

  test("Mod maintains the modulo of two variables.") {
    val bench = new InvBench(verbose)
    new Mod(bench.genIntVar(0 to 100),
      bench.genIntVar(1 to 100, true, (v: Int) => v != 0))
    bench.run
  }

  test("Abs maintains the absolute value of a variable.") {
    val bench = new InvBench(verbose)
    new Abs(bench.genIntVar(-100 to 100))
    bench.run
  }

  test("Step maintains a step function of the input var.") {
    val bench = new InvBench(verbose)
    new Step(bench.genIntVar(-100 to 100))
    bench.run
  }

  test("RoundUpModulo") {
    val bench = new InvBench(verbose)
    new RoundUpModulo(
      bench.genIntVar(0 to 10),
      bench.genIntVar(0 to 5),
      Gen.choose(10, 20).sample.get,
      Gen.choose(1, 5).sample.get,
      Gen.choose(0, 10).sample.get)
    bench.run
  }

 /* test("RoundUpCustom") {
    val bench = new InvBench(verbose)
    new RoundUpCustom(
      bench.genIntVar(0 to 10),
      bench.genIntVar(1 to 10),
      InvGen.randomTuples(10, 0 to 10).map(ab => (ab._1,ab._1 + ab._2)))
    bench.run
  }*/

  test("Union maintains the union of two sets.") {
    val bench = new InvBench(verbose)
    new Union(bench.genIntSetVar(), bench.genIntSetVar())
    bench.run
  }
  
  test("UnionAll maintains the union of a set of sets.") {
    val bench = new InvBench(verbose)
    new UnionAll(bench.genIntSetVars())
    bench.run
  }

  test("Inter maintains the intersection of two sets.") {
    val bench = new InvBench(verbose)
    new Inter(bench.genIntSetVar(), bench.genIntSetVar())
    bench.run
  }

  test("Diff maintains the difference between two sets.") {
    val bench = new InvBench(verbose)
    new Diff(bench.genIntSetVar(), bench.genIntSetVar())
    bench.run
  }

  test("Cardinality maintains the cardinality of a set.") {
    val bench = new InvBench(verbose)
    new Cardinality(bench.genIntSetVar())
    bench.run
  }

  test("MakeSet maintains an IntSetVar given a set of IntVar.") {
    val bench = new InvBench(verbose)
    new MakeSet(bench.genSortedIntVars(10, 0 to 10))
    bench.run
  }

  test("Interval maintains the set in the interval.") {
    val bench = new InvBench(verbose)
    new Interval(
      bench.genIntVar(-100 to 100),
      bench.genIntVar(-100 to 100))
    bench.run
  }

  test("TakeAny maintains a value taken from the set.") {
    val bench = new InvBench(verbose)
    new TakeAny(bench.genIntSetVar(), 0)
    bench.run
  }

  test("SetSum maintains the sum of variables (after optionnaly appliying a function).") {
    val bench = new InvBench(verbose)
    new SetSum(bench.genIntSetVar())
    bench.run
  }

  /**
   * Won't pass when the product products an overflow.
   */
  test("SetProd maintains the product of variables (after optionnaly appliying a function).") {
    val bench = new InvBench(verbose)
    new SetProd(bench.genIntSetVar(10, -3 to 3))
    bench.run
  }


/*  test("IdentityInt maintains the identity of an integer).") {
    val bench = new InvBench(verbose)
    new IdentityInt(bench.genIntVar(-100 to 100))
    bench.run
  }
*/

  /*
  test("IdentityIntSet maintains the identity of a set of integers).") {
    val bench = new InvBench(verbose)
    new IdentitySet(bench.genIntSetVar())
    bench.run
  }
  */
}

