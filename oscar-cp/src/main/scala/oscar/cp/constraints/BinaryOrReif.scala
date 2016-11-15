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
import oscar.algo.search.Outcome
import oscar.algo.search.Outcome._
import oscar.cp.core.variables.CPBoolVar

/**
 * x || y <--> b 
 * @author Pierre Schaus pschaus@gmail.com
 */
class BinaryOr(val x: CPBoolVar, val y: CPBoolVar, val b: CPBoolVar) extends Constraint(b.store, "BinaryOrReif") {

  
  override def setup(l: CPPropagStrength): Outcome = {
    x.callPropagateWhenBind(this)
    y.callPropagateWhenBind(this)
    b.callPropagateWhenBind(this)
    propagate()
  }

  override def propagate(): Outcome = {
    if (b.isBoundTo(0)) {
      if (x.assign(0) == Failure || y.assign(0) == Failure) Failure
      else Success
    } 
    else if (b.isBoundTo(1)) {
      if (x.isBoundTo(0)) y.assign(1)
      else if (y.isBoundTo(0)) x.assign(1)
      else Suspend
    } else {
      // b is not bound
      if (x.isBoundTo(1) || y.isBoundTo(1)) {
        if (b.assign(1) == Failure) Failure
        else Success
      } else if (x.isBoundTo(0) && y.isBoundTo(0)) {
        b.assign(0)
        Success
      } else {
        Suspend
      }
    }
  }
}


