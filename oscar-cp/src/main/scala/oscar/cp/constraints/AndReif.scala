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
import oscar.algo.reversible.ReversibleSparseSet
import oscar.algo.search.Outcome
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar

/**
 * and_i x_i <--> bi 
 * @author Pierre Schaus pschaus@gmail.com
 */
class And(val X: Array[CPBoolVar], val b: CPBoolVar) extends Constraint(b.store, "AndReif") {

  var unbound: ReversibleSparseSet = null
  priorityBindL1 = CPStore.MaxPriorityL1-1
  
  
  override def setup(l: CPPropagStrength): Outcome = {
    if (X.size == 2) {
      if (s.post(new BinaryAnd(X(0),X(1),b)) == Failure) return Failure
      else return Success
    }
    unbound = new ReversibleSparseSet(s,0,X.size-1)
    X.foreach(_.callPropagateWhenBind(this))
    for ((x,i) <- X.zipWithIndex) {
      if (x.isBound) {
        val oc = valBindIdx(x,i)
        if (oc == Failure || oc == Success) return oc
      }
      else x.callValBindIdxWhenBind(this,i)
    }
    b.callPropagateWhenBind(this)
    propagate()
  }
  
  override def valBindIdx(x: CPIntVar, idx: Int): Outcome = {
    if (x.isBoundTo(0)) {
      if (b.assign(0) == Failure) Failure
      else Success
    } else {
    	unbound.removeValue(idx)
    	if (unbound.isEmpty) {
    	  if (b.assign(1) == Failure) Failure
    	  else Success
    	}
    	else {
    	  Suspend
    	}
    }
  }
  
  override def propagate(): Outcome = {
    if (b.isBoundTo(1)) {
      for (i <- unbound) {
        if (X(i).assign(1) == Failure) return Failure
      }
      Success
    }
    else if (b.isBoundTo(0)) {
      // at least one must be = 0
      if (unbound.size == 1) {
        X(unbound.min).assign(0)
        Success
      }
      else Suspend
    }
    else Suspend

  }
}


