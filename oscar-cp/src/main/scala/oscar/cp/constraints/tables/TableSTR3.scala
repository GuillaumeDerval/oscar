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

package oscar.cp.constraints.tables

import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.{Constraint, CPOutcome, CPPropagStrength}
import oscar.cp.core.CPOutcome._
import oscar.algo.reversible.{ReversibleInt, ReversibleSharedSparseSet}

import scala.collection.mutable.{ArrayBuffer, HashSet}

/**
 * Implementation of the STR3 algorithm for the table constraint.
 * @param X the variables restricted by the constraint.
 * @param table the list of tuples composing the table.
 * @author Jordan Demeulenaere j.demeulenaere1@gmail.com
 * @author Guillaume Perez memocop@gmail.com
 * @author Pierre Schaus (pschaus@gmail.com)
 */
class TableSTR3(val X: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(X(0).store, "TableSTR3") {

  /* Basic information */
  private[this] val arity = X.length
  private[this] val store = X(0).store

  /* Temp arrays to fill domain values */
  private[this] val domainsFillArray = Array.fill(X.map(_.size).max)(0)
  private[this] var domainSize = 0

  /* Invalid tuples */
  private[this] val invalidTuples: ReversibleSharedSparseSet = new ReversibleSharedSparseSet(store, table.length)

  /* Supports for each (variable, value) pair */
  private[this] val originalMins = X.map(_.min)
  private[this] val spans = Array.tabulate(arity)(i => X(i).max - X(i).min + 1)
  private[this] val row = Array.tabulate(arity)(i => Array.fill(spans(i))(null: Array[Int]))
  private[this] val curr = Array.tabulate(arity)(i => Array.fill(spans(i))(null: ReversibleInt))
  private[this] val dep: Array[HashSet[Int]] = Array.fill(table.length)(HashSet[Int]())
  private[this] var depArray: Array[Int] = null
  private[this] val maxValue = X.map(_.max).max + 1

  /* ----- Setup ----- */

  /**
   * Check if a tuple is valid.
   * @param tuple the tuple.
   * @return true if the tuple is valid, false otherwise.
   */
  private final def isTupleValid(tuple: Array[Int]): Boolean = {
    var i = 0
    while (i < arity) {
      if (!X(i).hasValue(tuple(i))) {
        return false
      }
      i += 1
    }
    true
  }

  override def setup(l: CPPropagStrength): CPOutcome = {
    /* Fill temporary supports for each (x,a) pair */
    val tempSupport = Array.tabulate(arity)(i => Array.fill(spans(i))(new ArrayBuffer[Int]()))
    var t = 0
    while (t < table.length) {
      if (isTupleValid(table(t))) {
        var i = 0
        while (i < arity) {
          val a = table(t)(i)
          tempSupport(i)(a - originalMins(i)) += t
          i += 1
        }
      }
      t += 1
    }

    /* Fill row(x,a) and curr(x,a) for each variable value pair */
    var i = 0
    while (i < arity) {
      domainSize = X(i).fillArray(domainsFillArray)
      var j = 0
      while (j < domainSize) {
        val a = domainsFillArray(j)
        if (tempSupport(i)(a - originalMins(i)).isEmpty) {
          if (X(i).removeValue(a) == Failure) {
            return Failure
          }
        }
        else {
          curr(i)(a - originalMins(i)) = new ReversibleInt(store, tempSupport(i)(a - originalMins(i)).size - 1)
          val s = tempSupport(i)(a - originalMins(i)).toArray
          row(i)(a - originalMins(i)) = s
          dep(s(0)).add(i * maxValue + a)
        }
        j += 1
      }

      X(i).callOnChangesIdx(i, delta => valuesRemoved(delta))

      i += 1
    }
    depArray = Array.fill(dep.map(_.size).max)(0)
    Suspend
  }

  private final def valuesRemoved(delta: DeltaIntVar): CPOutcome = {
    val idx = delta.id
    var i = delta.fillArray(domainsFillArray)
    while (i > 0) {
      i -= 1
      val value = domainsFillArray(i)
      if (valueRemoved(X(idx),idx,value) == Failure) {
        return Failure
      }
    }
    Suspend
  }

  private final def valueRemoved(x: CPIntVar, idx: Int, value: Int): CPOutcome = {
    /* Invalidate supports of (x, a) */
    val prevInvSize = invalidTuples.size
    val originalMin = value - originalMins(idx)
    val currValue = curr(idx)(originalMin)
    val supports = row(idx)(originalMin)
    var supportIndex = 0
    while (supportIndex <= currValue) {
      val tupleIndex = supports(supportIndex)
      if (!invalidTuples.hasValue(tupleIndex)) {
        invalidTuples.insert(tupleIndex)
      }
      supportIndex += 1
    }

    /* No tuple has been invalidated */
    val newInvSize = invalidTuples.size
    if (newInvSize == prevInvSize) {
      return Suspend
    }

    /* Handle each invalidated tuple */
    var i = prevInvSize
    while (i < newInvSize) {
      val tupleIndex = invalidTuples(i)
      dep(tupleIndex).copyToArray(depArray)
      val depSize = dep(tupleIndex).size
      var depIndex = 0
      while (depIndex < depSize) {
        val depEntry = depArray(depIndex)
        val yIndex: Int = depEntry / maxValue
        val value = depEntry % maxValue
        if (X(yIndex).hasValue(value)) {
          /* Looking for a support for (yIndex, tuple(yIndex)) */
          val rowY = row(yIndex)(value - originalMins(yIndex))
          val currY = curr(yIndex)(value - originalMins(yIndex))

          val currValue = currY.value
          var p = currValue
          while (p >= 0 && invalidTuples.hasValue(rowY(p))) {
            p -= 1
          }

          /* If value has no more supports, delete from domain */
          if (p < 0) {
            if (X(yIndex).removeValue(value) == Failure) {
              return Failure
            }
            currY.value = -1
          }
          else {
            /* Check if we have to change curr value */
            if (p != currValue) {
              currY.setValue(p)
            }
            dep(tupleIndex).remove(depEntry)
            dep(rowY(p)).add(depEntry)
          }
        }
        depIndex += 1
      }
      i += 1
    }

    Suspend
  }

}