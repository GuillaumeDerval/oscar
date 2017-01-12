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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 *            Yoann Guyot
 ******************************************************************************/


package oscar.cbls.lib.invariant.minmax

import oscar.cbls.algo.heap.{ArrayMap, BinomialHeapWithMoveExtMem}
import oscar.cbls.core.computation.Invariant._
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.{Checker, KeyForElementRemoval}

import scala.collection.immutable.SortedSet

/**
 * Maintains {i in indices of (vars Inter cond) | vars[i] == max(vars(i in indices of (vars Inter cond))}
 * @param vars is an array of IntVar
 * @param cond is the condition, supposed fully acceptant if not specified
 * @param default is the value returned when cond is empty
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class ArgMax(vars: Array[IntValue], cond: SetValue = null, default: Int = Int.MinValue)
  extends ArgMiax(vars, cond, default) {

  override def Ord(v: IntValue): Int = -v.value
}

/**
 * Maintains {i in indices of (vars Inter cond) | vars[i] == min(vars(i in indices of (vars Inter cond))}
 * @param vars is an array of IntVar
 * @param cond is the condition, supposed fully accepting if not specified (must be specified if vars is bulked)
 * @param default is the value returned when cond is empty
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class ArgMin(vars: Array[IntValue], cond: SetValue = null, default: Int = Int.MaxValue)
  extends ArgMiax(vars, cond, default) {

  override def Ord(v: IntValue): Int = v.value
}

/**
 * Maintains {i in indices of (varss Inter cond) | varss[i] == miax(varss(i in indices of (varss Inter cond))}
 * Extact ordering is specified by implementiing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, can be null
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class ArgMiax(vars: Array[IntValue], cond: SetValue, default: Int)
  extends SetInvariant(initialDomain = vars.indices.start to vars.indices.last)
  with Bulked[IntValue, Unit]
  with VaryingDependencies
  with IntNotificationTarget
  with SetNotificationTarget{

  override def toString:String = {
    name + "(" + InvariantHelper.arrayToString(vars) + "," + cond + "," + default + ")"
  }

  var keyForRemoval: Array[KeyForElementRemoval] = new Array(vars.length)
  var h: BinomialHeapWithMoveExtMem[Int] = new BinomialHeapWithMoveExtMem[Int](i => Ord(vars(i)), vars.length, new ArrayMap(vars.length))

  if (cond != null) {
    registerStaticDependency(cond)
    registerDeterminingDependency(cond)
  }

  bulkRegister(vars)

  finishInitialization()

  if (cond != null) {
    for (i <- cond.value) {
      h.insert(i)
      keyForRemoval(i) = registerDynamicDependency(vars(i), i)
    }
  } else {
    for (i <- vars.indices) {
      h.insert(i)
      keyForRemoval(i) = registerDynamicDependency(vars(i), i)
    }
  }

  def Ord(v: IntValue): Int

  val firsts = h.getFirsts
  this := firsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
  var Miax = if (firsts.isEmpty) default else vars(h.getFirst).value

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Int, NewVal: Int) {
    //mettre a jour le heap
    h.notifyChange(index)

    if (vars(h.getFirst).value != Miax) {
      Miax = vars(h.getFirst).value
      this := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
    } else if (OldVal == Miax) {
      this.deleteValue(index)
      if (this.newValue.isEmpty) {

        for(first <- h.getFirsts){
          this :+= first
        }

        if (this.newValue.isEmpty) {
          Miax = default
        } else {
          Miax = vars(h.getFirst).value
        }
      }
    } else if (NewVal == Miax) {
      this.insertValue(index)
    }
  }

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == cond && cond != null)
    keyForRemoval(value) = registerDynamicDependency(vars(value), value)

    //mettre a jour le heap
    h.insert(value)

    if (vars(h.getFirst).value != Miax) {
      Miax = vars(h.getFirst).value
      this := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
    } else if (vars(value).value == Miax) {
      this.insertValue(value)
      Miax = vars(h.getFirst).value
    }
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == cond && cond != null)

    keyForRemoval(value).performRemove()
    keyForRemoval(value) = null

    //mettre a jour le heap
    h.delete(value)

    if (h.isEmpty) {
      Miax = default
      this := SortedSet.empty[Int]
    } else if (vars(h.getFirst).value != Miax) {
      Miax = vars(h.getFirst).value
      this := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
    } else if (vars(value).value == Miax) {
      this.deleteValue(value)
      if (this.newValue.isEmpty) {
        for(first <- h.getFirsts){
          this :+= first
        }

        Miax = vars(h.getFirst).value
      }
    }
  }

  override def checkInternals(c: Checker) {
    var count: Int = 0
    for (i <- vars.indices) {
      if (cond == null || (cond != null && cond.value.contains(i))) {
        if (vars(i).value == Miax) {
          c.check(this.value.contains(i),
            Some("this.value.contains(" + i + ")"))
          count += 1
        } else {
          c.check(Ord(Miax) < Ord(vars(i).value),
            Some("Ord(" + Miax + ") < Ord(vars(" + i + ").value ("
              + vars(i).value + "))"))
        }
      }
    }
    c.check(this.value.size == count, Some("this.value.size == count"))
    h.checkInternals(c: Checker)
    c.check(h.getFirsts.length == this.value.size, Some("h.getFirsts.length == this.value.size"))
    if (cond != null)
      c.check(this.value.subsetOf(cond.value), Some("this.newValue.subsetOf(cond.newValue)"))
  }
}

