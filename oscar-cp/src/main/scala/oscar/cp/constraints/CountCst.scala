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
import oscar.cp.core.CPOutcome._
import oscar.cp.core.variables.CPIntVar


/**
 * Implementation of Count Constraint:
 *   N variables of X take the values Y
 * @author Pierre Schaus pschaus@gmail.com
 */
class CountCst(val N: CPIntVar, val X: Array[CPIntVar], val Y: Int) extends Constraint(N.store, "CountSimple") {
  
 
  val n = X.size

  override def setup(l: CPPropagStrength): CPOutcome = {
    X.foreach(_.callPropagateWhenDomainChanges(this))
    N.callPropagateWhenBoundsChange(this)
    CPOutcome.Suspend
  }
  
  override def propagate(): CPOutcome = {
    var i = 0
    var sure = 0
    var possible = 0
    while (i < n) {
      if (X(i).isBoundTo(Y)) {
        sure += 1
      }
      if (X(i).hasValue(Y)) {
        possible += 1
      }
      i += 1
    }
    
    
    val minCount = sure
    val maxCount = possible
    
    if (N.updateMin(minCount) == CPOutcome.Failure) return CPOutcome.Failure
    if (N.updateMax(maxCount) == CPOutcome.Failure) return CPOutcome.Failure
    
    
    // we reached the maximum number values
    if (minCount == N.max) {
      i = 0
      while (i < n) {
        if (!X(i).isBound) {
          if (X(i).removeValue(Y) == CPOutcome.Failure) return CPOutcome.Failure
        }  
        i += 1
      }
      return CPOutcome.Success
    }
    // every value not surely equal to Y must be equal to Y
    if (maxCount == N.min) {
      i = 0
      while (i < n) {
        if (X(i).hasValue(Y)) {
          if (X(i).assign(Y) == CPOutcome.Failure) return CPOutcome.Failure
        }
        i += 1
      }
      return CPOutcome.Success
    }
    return CPOutcome.Suspend 
  }
  

}


