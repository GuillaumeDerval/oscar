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
package oscar.cbls.modeling

/**
 * ****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 *     Contributed to by Florent Ghilain
 * ****************************************************************************
 */

import oscar.cbls.constraints.lib.basic.{ BelongsTo, EQ, G, GE, L, LE, NE }
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.logic.{ ConstantIntElement, Elements, IntElement, SetElement }
import oscar.cbls.invariants.lib.numeric._
import oscar.cbls.invariants.lib.set._
import oscar.cbls.search.algo.InstrumentedRange

import scala.collection.immutable.SortedSet
import scala.language.implicitConversions

/**
 * Include this object whenever you want to use concise notation
 * It provides the following infix operators for IntVars: plus minus times, div, ==: !=: <<: >>: >=: <=:
 * @author renaud.delandtsheer@cetic.be
 */
object Algebra extends AlgebraTrait {
}

trait AlgebraTrait {

  // implicit conversion of Range towards a RangeHotRestart
  implicit def instrumentRange(r: Range): InstrumentedRange = new InstrumentedRange(r)

  implicit def InstrumentIntVar(v: IntValue): InstrumentedIntVar = new InstrumentedIntVar(v)

  implicit def InstrumentInt(a: Int): InstrumentedIntVar = InstrumentIntVar(CBLSIntConst(a))

  class InstrumentedIntVar(x: IntValue) {
    def +(v: IntValue): IntInvariant = Sum2(x, v)

    def -(v: IntValue): IntInvariant = Minus(x, v)

    def *(v: IntValue): IntInvariant = Prod(List(x, v))

    def /(v: IntValue): IntInvariant = Div(x, v)

    def %(v: IntValue): IntInvariant = Mod(x, v)

    def ===(v: IntValue) = new EQ(x, v)

    def !==(v: IntValue) = new NE(x, v)

    def >>=(v: IntValue) = new G(x, v)

    def <<=(v: IntValue) = new L(x, v)

    def >==(v: IntValue) = new GE(x, v)

    def le(v: IntValue) = new LE(x, v)

    def belongsTo(v: SetValue) = new BelongsTo(x, v)

    /**
     * creates a IntSEt maintained as the inclusive interval between te two variable
     * see [[oscar.cbls.invariants.lib.set.Interval]]
     * @param v
     * @return
     */
    def TO(v: CBLSIntVar) = new Interval(x, v)

    /*
     * if you write:
     * v <=s c:ConstraintSystem s= Expression
     *
     * and v has a domain that has bee ndeclared too small for the values that Expresson can produce
     * this will introduce a [[Bound]] to trim the values of Expression before assigning to v, and post constraints on Expression
     * into the constraintSystem c
     * @param c
     * @return
     */
    //def `<=s`(c: ConstraintSystem) = new SafeAssignment(x,c)
  }

  /*  class SafeAssignment(v:CBLSIntVar, c:ConstraintSystem){
    //TODO: trouver un vrais nom de méthode
    def `s=`(i:IntInvariant){
      val iMax = i.max
      val iMin = i.min
      if(iMax <= v.max && iMin >= v.min){
        v <== i
      }else{
        v <== Bound(i, v.min, v.max)
        c.add(i >== v.min)
        c.add(v.max >== i)
      }
    }

    def `s=`(w:CBLSIntVar){
      if(w.max<= v.max && w.min >= v.min){
        v <== w
      }else{
        v <== Bound(w, v.min, v.max)
        c.add(w >== v.min)
        c.add(v.max >== w)
      }
    }
  }
*/
  implicit def InstrumentIntSetVar(v: SetValue): InstrumentedIntSetVar = new InstrumentedIntSetVar(v)

  implicit def InstrumentIntSet(a: SortedSet[Int]): InstrumentedIntSetVar = InstrumentIntSetVar(CBLSSetConst(a))

  class InstrumentedIntSetVar(x: SetValue) {
    def union(v: SetValue): SetInvariant = Union(x, v)

    def inter(v: SetValue): SetInvariant = Inter(x, v)

    def minus(v: SetValue): SetInvariant = Diff(x, v)

    def map(fun: Int => Int, myMin: Int = Int.MinValue, myMax: Int = Int.MaxValue) = SetMap(x, fun, myMin to myMax)

  }

  implicit def InstrumentArrayOfIntValue(inputarray: Array[IntValue]): InstrumentedArrayOfIntValue = new InstrumentedArrayOfIntValue(inputarray)

  class InstrumentedArrayOfIntValue(inputarray: Array[IntValue]) {
    def element(index: IntValue) = IntElement(index, inputarray)

    def elements(index: SetValue) = Elements(index, inputarray)
  }

  implicit def instrumentArrayOfSetValue(inputarray: Array[SetValue]): InstrumentedArrayOfSetValue = new InstrumentedArrayOfSetValue(inputarray)

  class InstrumentedArrayOfSetValue(inputarray: Array[SetValue]) {
    def apply(index: IntValue): SetInvariant = SetElement(index, inputarray)
  }

  implicit def InstrumentArrayOfIntConstants(inputarray: Array[Int]): InstrumentedArrayOfIntConstants = new InstrumentedArrayOfIntConstants(inputarray)

  class InstrumentedArrayOfIntConstants(inputarray: Array[Int]) {
    def element(index: IntValue) = ConstantIntElement(index, inputarray)

    def elements(index: SetValue) = Elements(index, inputarray.map(CBLSIntConst.apply))
  }

  @deprecated("You are implicitly converting Array[Int] to Array[CBLSIntconst]. This is OK, but prevents Bulking. You should create an array of CBLSIntConst straight, and use this array everywhere you need it", "always")
  implicit def arrayOfInt2ArrayOfIntValue(a: Array[Int]): Array[IntValue] = a.map(CBLSIntConst.apply)

  implicit def arrayOfIntVar2ArrayOfIntValue(a: Array[CBLSIntVar]): Array[IntValue] = a.asInstanceOf[Array[IntValue]]
  implicit def arrayOfIntVar2ArrayOfInstrumentedIntValue(a: Array[CBLSIntVar]): InstrumentedArrayOfIntValue = new InstrumentedArrayOfIntValue(a.asInstanceOf[Array[IntValue]])

  implicit def arrayOfSetVar2ArrayOfSetValue(a: Array[CBLSSetVar]): Array[SetValue] = a.asInstanceOf[Array[SetValue]]
  implicit def arrayOfSetVar2ArrayOfInstrumentedSetValue(a: Array[CBLSSetVar]): InstrumentedArrayOfSetValue = new InstrumentedArrayOfSetValue(a.asInstanceOf[Array[SetValue]])

  implicit def arrayOfIntInvariant2ArrayOfIntValue(a: Array[IntInvariant]): Array[IntValue] = a.asInstanceOf[Array[IntValue]]
  implicit def arrayOfIntInvariant2ArrayOfInstrumentedIntValue(a: Array[IntInvariant]): InstrumentedArrayOfIntValue = new InstrumentedArrayOfIntValue(a.asInstanceOf[Array[IntValue]])

  implicit def arrayOfIntConst2ArrayOfIntValue(a: Array[CBLSIntConst]): Array[IntValue] = a.asInstanceOf[Array[IntValue]]
  implicit def arrayOfIntconst2ArrayOfInstrumentedIntValue(a: Array[CBLSIntConst]): InstrumentedArrayOfIntValue = new InstrumentedArrayOfIntValue(a.asInstanceOf[Array[IntValue]])

  implicit def sortedSetConstToIntValue(a: SortedSet[CBLSIntConst]): SortedSet[IntValue] = a.asInstanceOf[SortedSet[IntValue]]
  implicit def sortedSetVarToIntValue(a: SortedSet[CBLSIntVar]): SortedSet[IntValue] = a.asInstanceOf[SortedSet[IntValue]]
}

