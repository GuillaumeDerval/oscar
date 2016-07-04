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
 * x + y = z 
 * @author Pierre Schaus pschaus@gmail.com
 * @author Steven Gay steven.gay@uclouvain.be
 * 
 */
class BinarySum(val x: CPIntVar, val y: CPIntVar, val z: CPIntVar) extends Constraint(x.store, "BinarySum") {

  idempotent = true
  override def setup(l: CPPropagStrength): CPOutcome = {
    priorityL2 = CPStore.MaxPriorityL2 - 1
    if (!x.isBound) x.callPropagateWhenBoundsChange(this)
    if (!y.isBound) y.callPropagateWhenBoundsChange(this)
    if (!z.isBound) z.callPropagateWhenBoundsChange(this)
    propagate()
  }

  override def propagate(): CPOutcome = {
    // While reading this code, remember that there are sorting networks in Java's default sort. Check and compare, this is not so bad.
    // The logic itself is much lighter than the CP variable manipulations, so we add logic to do the fewest manipulations we can.
    
    // min and max are expensive, better remember values than recompute
    var xmax = x.max ; var xmin = x.min
    var ymax = y.max ; var ymin = y.min
    var zmax = z.max ; var zmin = z.min
    
    var reduce = true // for idempotence
    var newbound = 0
    
    while (reduce) {
      reduce = false
      
      newbound = xmax + ymax
      if (zmax > newbound) { // update only if necessary
        if(z.updateMax(newbound) == Failure) return Failure
        zmax = z.max
        if(zmax < newbound)  // if zmax is a multiplicative or non bijective view, recompute for idempotence 
          reduce = true
      }

      newbound = zmax - ymin
      if (xmax > newbound) {
        if(x.updateMax(newbound) == Failure) return Failure
        xmax = x.max
        if (xmax < newbound) 
          reduce = true
      }

      newbound = zmax - xmin
      if (ymax > newbound) {
        if(y.updateMax(newbound) == Failure) return Failure
        ymax = y.max
        if (ymax < newbound) 
          reduce = true
      }

      newbound = xmin + ymin
      if (zmin < newbound) {
        if(z.updateMin(newbound) == Failure) return Failure
        zmin = z.min
        if (zmin > newbound) 
          reduce = true
      }
      
      newbound = zmin - ymax
      if (xmin < newbound) {
        if(x.updateMin(newbound) == Failure) return Failure
        xmin = x.min
        if (xmin > newbound) 
          reduce = true
      }
            
      newbound = zmin - xmax
      if (ymin < newbound) {
        if(y.updateMin(newbound) == Failure) return Failure
        ymin = y.min
        if (ymin > newbound) 
          reduce = true
      }
    }
    Suspend
    // Fine-tuning is relaxing for the mind.
    // Except for all this red, it feels like M. Odersky himself punishes my eyes for using so many mutables.  
  }
}


