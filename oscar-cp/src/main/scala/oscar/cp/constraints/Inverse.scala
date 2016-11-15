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
package oscar.cp.constraints

import oscar.algo.search.Outcome
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.algo.search.Outcome._
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.CPStore

/**
 * Inverse
 *
 *  This constraint enforces the following rules:
 *  1. prev(next(i)) == i
 *  2. next(prev(i)) == i
 *
 *  @author Renaud Hartert ren.hartert@gmail.com
 */

class Inverse(prev: Array[CPIntVar], next: Array[CPIntVar]) extends Constraint(prev.head.store, "Inverse") {

  // Checks the consistency of the arguments
  require(prev.length == next.length, "input arrays must have the same size")

  // Structure used to collect removed values
  private[this] val removedValues = new Array[Int](prev.length)

  override def setup(l: CPPropagStrength): Outcome = {
    if (init() == Failure) Failure
    else {
      var i = prev.length
      while (i > 0) {
        i -= 1
        if (!prev(i).isBound) prev(i).callOnChangesIdx(i, s => propagatePrev(s))
        if (!next(i).isBound) next(i).callOnChangesIdx(i, s => propagateNext(s))
      }
      Suspend
    }
  }

  @inline private def propagatePrev(delta: DeltaIntVar): Outcome = {
    val varId = delta.id
    val intVar = prev(varId)
    if (intVar.isBound) next(intVar.min).assign(varId)
    else {
      var i = delta.fillArray(removedValues)
      while (i > 0) {
        i -= 1
        val value = removedValues(i)
        if (next(value).removeValue(varId) == Failure) return Failure
      }
      Suspend
    }
  }

  @inline private def propagateNext(delta: DeltaIntVar): Outcome = {
    val varId = delta.id
    val intVar = next(varId)
    if (intVar.isBound) prev(intVar.min).assign(varId)
    else {
      var i = delta.fillArray(removedValues)
      while (i > 0) {
        i -= 1
        val value = removedValues(i)
        if (prev(value).removeValue(varId) == Failure) return Failure
      }
      Suspend
    }
  }
  
  @inline private def init(): Outcome = {
    var i = 0
    while (i < prev.length) {
      // Initializes the bounds of the variables
      if (!initBounds(prev(i))) return Failure
      else if (!initBounds(next(i))) return Failure
      else {
        var j = 0
        while (j < prev.length) {
          // Initializes inner domains
          if (!init(prev, next, i, j)) return Failure
          else if (!init(next, prev, i, j)) return Failure
          else j += 1
        }
      }
      i += 1
    }
    Suspend
  }

  @inline private def initBounds(intVar: CPIntVar): Boolean = {
    if (intVar.updateMin(0) == Failure) false
    else if (intVar.updateMax(prev.length - 1) == Failure) false
    else true
  }

  @inline private def init(vector1: Array[CPIntVar], vector2: Array[CPIntVar], i: Int, j: Int): Boolean = {
    if (!vector1(i).hasValue(j)) true
    else if (vector1(i).isBound) vector2(j).assign(i) != Failure
    else if (!vector2(j).hasValue(i)) vector1(i).removeValue(j) != Failure
    else true
  }
}

