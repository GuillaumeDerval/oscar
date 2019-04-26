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

package oscar.cbls.lib.invariant.set

import oscar.cbls._
import oscar.cbls.core._
import scala.collection.immutable.SortedSet

/**
 * Sum(i in on)(fun(i))
 * @param on is the set of integers to add
 * @param fun is an optional function Long -> Long to apply before summing elements. It is expected not to rely on any variable of the model.
 * @author renaud.delandtsheer@cetic.be
 * */
case class SetSum(on: SetValue, fun: (Long => Long) = (a: Long) => a)
  extends IntInvariant(on.value.foldLeft(0L)((a, b) => a + fun(b)))
  with SetNotificationTarget{

  registerStaticAndDynamicDependency(on)
  finishInitialization()

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Long], removedValues: Iterable[Long], oldValue: SortedSet[Long], newValue: SortedSet[Long]): Unit = {
    var delta = 0L
    for (added <- addedValues) delta += fun(added)
    for (deleted <- removedValues) delta -= fun(deleted)
    this :+= delta
  }

  override def checkInternals(c: Checker) {
    var count = 0L
    for (v <- on.value) count += fun(v)
    c.check(this.value == count, Some("this.value == count"))
  }
}

/**
 * PRod(i in on)(fun(i))
 * @param on is the set of integers to multiply
 * @author renaud.delandtsheer@cetic.be
 * */
case class SetProd(on: SetValue)
  extends IntInvariant
  with SetNotificationTarget{

  registerStaticAndDynamicDependency(on)
  finishInitialization()

  var nonZeroProduct = on.value.foldLeft(1L)(
    (acc, value) => if (value == 0L) { acc } else { acc * value})

  if (on.value.contains(0L)) {
    this := 0L
  } else {
    this := nonZeroProduct
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Long], removedValues: Iterable[Long], oldValue: SortedSet[Long], newValue: SortedSet[Long]): Unit = {
    for (deleted <- removedValues) if (deleted != 0L) {nonZeroProduct /= deleted}
    for (added <- addedValues) if (added != 0L) {nonZeroProduct *= added}
    if (newValue.contains(0L)) {
      this := 0L
    } else {
      this := nonZeroProduct
    }
  }

  override def checkInternals(c: Checker) {
    var countNZ = 1L
    for (v <- on.value) if(v !=0L) countNZ *= v
    c.check(nonZeroProduct == countNZ,
      Some("non zero product (" + nonZeroProduct + ") == product of non zero items (" + countNZ + ") and on is " + on))

    var count = 1L
    for (v <- on.value) count *= v
    c.check(this.value == count,
      Some("this.value (" + this.value + ") == count (" + count + ") and on is " + on))
  }
}
