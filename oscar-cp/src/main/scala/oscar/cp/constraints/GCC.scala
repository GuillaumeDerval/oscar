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
import oscar.cp.core.CPOutcome._
import oscar.cp.constraints._
import oscar.cp.modeling._
import oscar.algo.DisjointSets
import scala.collection.mutable.ArrayBuffer
import oscar.cp.core.variables.CPIntVar

/**
 * Global Cardinality Constraint
 * Constraint the values minval+i to appear between low[i] and up[i] times in x
 * @author Pierre Schaus pschaus@gmail.com
 */
class GCC(x: Array[CPIntVar], minval: Int, low: Array[Int], up: Array[Int]) extends Constraint(x(0).store) {
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    
    val ok = l match {
      case CPPropagStrength.Weak => {
        s.post(new GCCFWC(x, minval, low, up)) 
      }
      case CPPropagStrength.Medium => {
        s.post(Array(new GCCUpperBC(x, minval, up), new GCCFWC(x, minval, low, up)))
      }
      case CPPropagStrength.Strong => s.post(new SoftGCC(x, minval, low, up, CPIntVar(0,0)(s)))
      case CPPropagStrength.Automatic => {
        s.post(Array(new GCCUpperBC(x, minval, up), new GCCFWC(x, minval, low, up)))
      }
      
    }
    if (ok == CPOutcome.Failure) return CPOutcome.Failure;
    else return CPOutcome.Success;

  }
  

}
