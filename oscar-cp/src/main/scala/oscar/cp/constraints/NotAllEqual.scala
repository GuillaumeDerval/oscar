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
package oscar.cp.constraints

import oscar.algo.reversible.{ReversibleBoolean, ReversibleInt}
import oscar.algo.search.Outcome
import oscar.cp.core._
import oscar.cp.core.variables.CPIntVar

/**
 * Ensure that at least two values in x are different (they are not all equal)
 * @author Guillaume Derval <guillaume.derval@uclouvain.be>
 */
class NotAllEqual(val x: Array[CPIntVar]) extends Constraint(x(0).store, "NotAllEqual") {
  val firstValueFound = new ReversibleBoolean(s,false)
  var firstValue: Int = 0
  val nUnbound = new ReversibleInt(s,x.length)

  override def setup(l: CPPropagStrength): Outcome = {
    // Check specific cases
    if(x.length == 1)
      return Outcome.Success
    if(x.length == 2) {
      if(x(0).store.post(x(0).diff(x(1))) == Outcome.Failure)
        return Outcome.Failure
      return Outcome.Success
    }

    x.zipWithIndex.foreach{case (v, idx) => {
      v.callValBindIdxWhenBind(this, idx)

      // If the variable is already bound, call valBindIdx
      if(v.isBound && valBindIdx(v, idx) == Outcome.Success)
        return Outcome.Success
    }}

    if(nUnbound.getValue() == 0)
      Outcome.Failure //if we have all our variable bound but did not return Success earlier, we failed
    else
      Outcome.Suspend
  }

  override def valBindIdx(x: CPIntVar, idx: Int): Outcome = {
    nUnbound.decr()
    if(firstValueFound.getValue()) { //if we already found our first value
      if(x.min != firstValue)
        return Outcome.Success
    }
    else {
      firstValueFound.setTrue()
      firstValue = x.min
    }
    if(nUnbound.getValue() == 0)
      Outcome.Failure
    else
      Outcome.Suspend
  }
}


