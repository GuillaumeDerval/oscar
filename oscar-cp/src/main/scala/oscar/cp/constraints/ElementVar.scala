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


package oscar.cp.constraints;

import oscar.algo.search.Outcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint

import scala.math.min
import scala.math.max
import oscar.cp.core._
import oscar.algo.search.Outcome._
import oscar.cp.core.CPSolver


/**
 * Element Constraint: y(x) == z
 * Use CPPropagStrength.Strong to have GAC propagation, otherwise BC propagation is used.
 * @author Pierre Schaus - pschaus@gmail.com
 */
class ElementVar(val y: Array[CPIntVar], val x: CPIntVar, val z: CPIntVar) extends Constraint(y(0).store, "ElementVar") {

  override def setup(l: CPPropagStrength): Outcome = {
    if (l == CPPropagStrength.Strong) {
      //if (s.post(new ElementVarAC(y,x,z)) == Failure) return Failure
      if (s.post(new ElementVarAC3(y,x,z)) == Failure) return Failure
      else Success
    } else {
      if (s.post(new ElementVarBC(y,x,z)) == Failure) return Failure
      else Success      
    }

  }
  
 

}
