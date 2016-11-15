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


/**
 * Implementation of Count Constraint:
 *   N variables of X take the values Y
 * @author Pierre Schaus pschaus@gmail.com
 */
class CountSimple(val N: CPIntVar, val X: Array[CPIntVar], val Y: CPIntVar) extends Constraint(N.store, "CountSimple") {
  
  val n = X.size
  val eqY = X.map(i => i)
  val nEqY = new ReversibleInt(s, 0)
  val diffY = X.map(i => i)
  val nDiffY = new ReversibleInt(s, 0)
  
  
  private def setEq(i: Int) {
    val tmp = eqY(nEqY.value)
    eqY(nEqY.value) = eqY(i)
    eqY(i) = tmp
    nEqY.incr()
  }
  
  private def setDiff(i: Int) {
    val tmp = diffY(nDiffY.value)
    diffY(nDiffY.value) = diffY(i)
    diffY(i) = tmp
    nDiffY.incr()
  }  
  

  override def setup(l: CPPropagStrength): Outcome = {
    X.foreach(_.callPropagateWhenDomainChanges(this))
    Y.callPropagateWhenBind(this)
    N.callPropagateWhenBoundsChange(this)
    Outcome.Suspend
  }
  
  override def propagate(): Outcome = {
    var i = 0
    if (Y.isBound) {
      val v = Y.min
      var i = nEqY.value
      while (i < n) {
        if (eqY(i).isBoundTo(v)) setEq(i)
        i += 1
      }
      i = nDiffY.value
      while (i < n) {
        if (!diffY(i).hasValue(v)) setDiff(i)
        i += 1
      }
    } else  {
      i = nDiffY.value
      while (i < n) {
        if (diffY(i).max < Y.min || diffY(i).min > Y.max) setDiff(i)
        i += 1
      }
    }
    
    
    val minCount = nEqY.value
    val maxCount = n-nDiffY.value
    
    if (N.updateMin(minCount) == Outcome.Failure) return Outcome.Failure
    if (N.updateMax(maxCount) == Outcome.Failure) return Outcome.Failure
    
    
    // we reached the maximum number values
    if (minCount == N.max && Y.isBound) {
      val v = Y.min
      i = nEqY.value
      while (i < n) {
        if (eqY(i).removeValue(v) == Outcome.Failure) return Outcome.Failure
        i += 1
      }
      return Outcome.Success
    }
    // every value not surely equal to Y must be equal to Y
    if (maxCount == N.min && Y.isBound) {
      val v = Y.min
      i = nDiffY.value
      while (i < n) {
        if (diffY(i).assign(v) == Outcome.Failure) return Outcome.Failure
        i += 1
      }
      return Outcome.Success
    }
    return Outcome.Suspend
  }
  

}


