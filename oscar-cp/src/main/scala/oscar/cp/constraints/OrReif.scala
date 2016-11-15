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

/**
 * y is true if at least one of the xi's is true, false otherwise
 * (or x_i) == y
 * @author Pierre Schaus pschaus@gmail.com
 */
class OrReif(val X: Array[CPBoolVar], y: CPBoolVar) extends Constraint(y.store, "OrReif") {

  val x = X.map(i => i)
  val nFalse = new ReversibleInt(s,0)
  var i = 0
  val n = x.size
  
  private def setBound(i: Int) {
    
    val tmp = x(nFalse.value)
    x(nFalse.value) = x(i)
    x(i) = tmp
    nFalse.incr()
  }
  priorityL2 = CPStore.MaxPriorityL2
  
  override def setup(l: CPPropagStrength): Outcome = {
	  if (x.size == 2) {
	        if (s.post(new BinaryOr(x(0),x(1),y)) == Failure) return Failure;
	        else return Success;
	  }
	  i = 0
	  while (i < n) {
		  if (x(i).isFalse) setBound(i)
		  i += 1
	  }
	  val ok = propagate()
	  if (ok != Suspend) return ok
	  for (z <- x; if !z.isBound) 
	    z.callPropagateWhenBind(this)
	  if (!y.isBound) y.callPropagateWhenBind(this)
	  Suspend
  }


  override def propagate(): Outcome = {

    if (y.isFalse) {
      i = nFalse.value
      while (i < n) {
    	if (x(i).assign(0) == Failure) {
    	  return Failure
    	}
        i += 1
      }
      return Success
    }
    i = nFalse.value
    while (i < n) {
      if (x(i).isFalse) setBound(i)
      else if (x(i).isTrue) {
        if (y.assign(1) == Failure) {
          return Failure
        }
        else return Success
      }
      i += 1
    }
    if (nFalse.value == n) {
      if (y.assign(0) == Failure) {
        return Failure
      }
    }
    if (y.isTrue && nFalse.value == n-1) {
      if (x(n-1).assign(1) == Failure) return Failure
    }
    Suspend

  }
}


