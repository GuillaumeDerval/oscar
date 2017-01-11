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
/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 * ****************************************************************************
 */

package oscar.cbls.lib.invariant.minmax
/**This package proposes a set of logic invariants, which are used to define the structure of the problem*/

import oscar.cbls.algo.heap._
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.logic._

import scala.collection.immutable.SortedSet

abstract class MiaxLin(vars: SortedSet[IntValue])
  extends IntInvariant(initialValue = 0)
  with IntNotificationTarget{

  require(vars.size > 0, "Invariant " + this + " declared with zero vars to max")

  for (v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  restrictDomain(vars.foldLeft((vars.head.min, vars.head.max))((acc, intvar) =>
    (if (better(intvar.min, acc._1)) intvar.min else acc._1,
      if (better(intvar.max, acc._2)) intvar.max else acc._2)))

  var MiaxCount: Int = 0

  def better(a: Int, b: Int): Boolean //true if a is strictly more in the direction of the invariant that b

  private def LoadNewMiax() {
    var CurrentMiax: Int = vars.head.value
    MiaxCount = 1
    vars.foreach(v => {
      if (v.value == CurrentMiax) {
        MiaxCount += 1
      } else if (better(v.value, CurrentMiax)) {
        MiaxCount = 1
        CurrentMiax = v.value
      }
    })
    this := CurrentMiax
  }

  LoadNewMiax()

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    assert(vars.contains(v), this + " notified for not interesting var")
    val MiaxVal = this.newValue
    if (OldVal == MiaxVal && better(MiaxVal, NewVal)) {
      MiaxCount -= 1
      if (MiaxCount == 0) LoadNewMiax() //this is where we pay the price.
    } else if (better(NewVal, MiaxVal)) {
      MiaxCount = 1
      this := NewVal
    } else if (better(MiaxVal, OldVal) && NewVal == MiaxVal) {
      MiaxCount += 1
    }
  }

  override def checkInternals(c: Checker) {
    vars.foreach(v => c.check(better(this.value, v.value) || this.value == v.value,
      Some("better(output.value (" + this.value + "), " + v.value
        + ") || output.value == " + v.value)))
  }
}

/**
 * maintains output = Max(vars)
 * where
 * * output is an IntVar
 * * on is a set of IntVar
 * update is O(n)
 * @author renaud.delandtsheer@cetic.be
 * */
case class MaxLin(vars: SortedSet[IntValue]) extends MiaxLin(vars) {

  override def better(a: Int, b: Int): Boolean = a > b
}

/**
 * maintains output = Min(vars)
 * where
 * * output is an IntVar
 * * on is a set of IntVar
 * update is O(n)
 * @author renaud.delandtsheer@cetic.be
 * */
case class MinLin(vars: SortedSet[IntValue]) extends MiaxLin(vars) {

  override def better(a: Int, b: Int): Boolean = a < b
}

abstract class Miax(vars: SortedSet[IntValue])
  extends IntInvariant
  with IntNotificationTarget{

  for (v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  restrictDomain(vars.foldLeft((vars.head.min, vars.head.max))((acc, intvar) =>
    (if (better(intvar.min, acc._1)) intvar.min else acc._1,
      if (better(intvar.max, acc._2)) intvar.max else acc._2)))

  def ord(v: IntValue): Int
  def better(a: Int, b: Int): Boolean

  val h: BinomialHeapWithMove[IntValue] = new BinomialHeapWithMove[IntValue](ord, vars.size)

  for (v <- vars) { h.insert(v) }

  this := h.getFirst.value

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    assert(vars.contains(v), name + " notified for not interesting var")
    h.notifyChange(v)
    this := h.getFirst.value
  }

  override def checkInternals(c: Checker) {
    vars.foreach(v => c.check(better(this.value, v.value)
      || this.value == v.value,
      Some("better(this.value (" + this.value + "), " + v.value
        + ") || this.value == " + v.value)))
  }
}

/**
 * maintains output = Min(vars)
 * deprecated: use MinArray
 * where
 * * output is an IntVar
 * update is O(n*n)
 * @author renaud.delandtsheer@cetic.be
 * */
@deprecated("use the MinArray instead", "always")
case class Min(vars: SortedSet[IntValue]) extends Miax(vars) {
  assert(vars.size > 0, "Invariant Min declared with zero vars to min")

  override def ord(v: IntValue): Int = v.value

  override def better(a: Int, b: Int): Boolean = a < b
}

object Min{
  def apply(varss: Array[IntValue], ccond: SetValue = null, default: Int = Int.MaxValue) = MinArray(varss, ccond, default)
}

/**
 * maintains output = Max(vars)
 * deprecated use MaxArray
 * where
 * * output is an IntVar
 * update is O(n*n)
 * @author renaud.delandtsheer@cetic.be
 * */
@deprecated("use the MaxArray instead", "always")
case class Max(vars: SortedSet[IntValue]) extends Miax(vars) {
  assert(vars.size > 0, "Invariant Max declared with zero vars to max")

  override def ord(v: IntValue): Int = -v.value

  override def better(a: Int, b: Int): Boolean = a > b
}

object Max{
  def apply(varss: Array[IntValue], ccond: SetValue = null, default: Int = Int.MinValue) = MaxArray(varss, ccond, default)
}

/**
 * maintains output = Max(a,b)
 * where output, a, and b are an IntVar
 * use this if you only have two variables to max, otherwise, refer to log implementations
 * @author renaud.delandtsheer@cetic.be
 * */
case class Max2(a: IntValue, b: IntValue)
  extends IntInt2Int(a, b, (x: Int, y: Int) => x.max(y), a.min.max(b.min) to a.max.max(b.max))

/**
 * maintains output = Min(a,b)
 * where output, a, and b are an IntVar
 * use this if you only have two variables to max, otherwise, refer to log implementations
 * @author renaud.delandtsheer@cetic.be
 * */
case class Min2(a: IntValue, b: IntValue)
  extends IntInt2Int(a, b, (x: Int, y: Int) => x.min(y), a.min.min(b.min) to a.max.min(b.max))
