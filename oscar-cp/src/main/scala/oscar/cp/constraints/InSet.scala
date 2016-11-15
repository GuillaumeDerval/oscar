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

import oscar.cp.core._
import oscar.algo.reversible._
import oscar.algo.search.Outcome._
import oscar.algo.search.Outcome
import oscar.cp.core.variables.CPIntVar

/**
 * x must be a value of the set
 * @author Pierre Schaus pschaus@gmail.com
 */
class InSet(val x: CPIntVar, val set: Set[Int]) extends Constraint(x.store, "InSet") {
  
  override def setup(l: CPPropagStrength): Outcome = {
    for (v <- x.min to x.max if !set.contains(v)) {
      if (x.removeValue(v) == Failure) {
        return Failure
      }
    }
    return Success
  }
 
}


