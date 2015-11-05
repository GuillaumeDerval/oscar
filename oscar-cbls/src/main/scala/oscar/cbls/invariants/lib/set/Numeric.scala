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
 *            Yoann Guyot
 * ****************************************************************************
 */

package oscar.cbls.invariants.lib.set

import oscar.cbls.invariants.core.computation.{ChangingSetValue, IntInvariant, SetValue}
import oscar.cbls.invariants.core.propagation.Checker

/**
 * Sum(i in on)(fun(i))
 * @param on is the set of integers to add
 * @param fun is an optional function Int -> Int to apply before summing elements. It is expected not to rely on any variable of the model.
 * @author renaud.delandtsheer@cetic.be
 * */
case class SetSum(on: SetValue, fun: (Int => Int) = (a: Int) => a)
  extends IntInvariant(on.value.foldLeft(0)((a, b) => a + fun(b))) {

  registerStaticAndDynamicDependency(on)
  finishInitialization()

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == on)
    this :+= fun(value)
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == on)
    this :-= fun(value)
  }

  override def checkInternals(c: Checker) {
    var count = 0
    for (v <- on.value) count += fun(v)
    c.check(this.value == count, Some("this.value == count"))
  }
}

/**
 * PRod(i in on)(fun(i))
 * @param on is the set of integers to multiply
 * @param fun is an optional function Int -> Int to apply before multiplying elements. It is expected not to rely on any variable of the model.
 * @author renaud.delandtsheer@cetic.be
 * */
case class SetProd(on: SetValue, fun: (Int => Int) = (a: Int) => a) extends IntInvariant {

  var NonZeroProduct: Int = 0

  registerStaticAndDynamicDependency(on)
  finishInitialization()

  NonZeroProduct = on.value.foldLeft(1)(
    (acc, value) => if (value == 0) { acc } else { acc * fun(value) })
  if (on.value.contains(0)) {
    this := 0
  } else {
    this := NonZeroProduct
  }

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == on)
    if (value != 0) {
      NonZeroProduct *= fun(value)
    }
    if (on.value.contains(0)) {
      this := 0
    } else {
      this := NonZeroProduct
    }
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == on, "The given set (IntSetVar) should be SetProd.on.")
    if (value != 0) {
      NonZeroProduct /= fun(value)
    }
    if (on.value.contains(0)) {
      /**
       * Nothing to do since 0 is already in this
       * or it will be added when its insertion in the set will be notified.
       */
    } else {
      this := NonZeroProduct
    }
  }

  override def checkInternals(c: Checker) {
    var count = 1
    for (v <- on.value) count *= v
    c.check(this.value == count,
      Some("this.value (" + this.value + ") == count (" + count + ")"))
  }
}
