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
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPBoolVar

/**
 * Implementation of sum_i a(i).x(i) == c <--> b
 * @author Pierre Schaus pschaus@gmail.com
 */
class WeightedSumReif(val a: Array[Int], val x: Array[CPIntVar], val c: Int, val b: CPBoolVar) extends Constraint(b.store, "WeightedSumReif") {

  override def setup(l: CPPropagStrength): Outcome = {
    x.foreach(_.callPropagateWhenDomainChanges(this))
    b.callPropagateWhenBind(this)
    Suspend
  }

  override def propagate(): Outcome = {
    if (b.isBoundTo(1)) {
      if (s.post(new oscar.cp.constraints.WeightedSum(a,x,CPIntVar(c)(s))) == Failure) Failure
      else Success
    } else {
      val m = a.zip(x).map{case(ai,xi) => if (ai < 0) ai*xi.max else ai*xi.min}.sum
      if (m > c) {
        if (b.assign(0) == Failure) return Failure
        else return Success
      }
      val M = a.zip(x).map{case(ai,xi) => if (ai < 0) ai*xi.min else ai*xi.max}.sum
      if (M < c) {
        if (b.assign(0) == Failure) return Failure
        else return Success
      }
      if (m == M) {
        if (b.assign(1) == Failure) return Failure
        else return Success
      }
      Suspend
    }
    
  }

}


