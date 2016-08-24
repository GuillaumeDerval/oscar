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

import oscar.algo.reversible._
import oscar.cp.core.CPOutcome
import oscar.cp._
import scala.collection.JavaConversions._
import oscar.cp.core.CPPropagStrength

/**
 * Implementation of Path Constraint decomposition as described in 
 * "Solving the Longest Simple Path Problem with Constraint-Based Techniques" by Quang Dung, Yves Deville (CPAIOR2012)
 * 
 * - succ[i] is the successor of node i (also place i inside the domain of succ[i] if you want to allow it not to be part of the path
 * - start is the index of the first node on the path
 * - end is the index of the last node on the path
 * - length is the length of the path (number edges)
 * 
 * Example: 
 * succ [1, 3, 2, 5, 4, 0], start = 0, end = 5, length = 3 represents the path 0 -> 1 -> 3 -> 5
 * Notice that nodes that do not belong to the path, have them-self as successor and that 
 * the successor of the last node of the path is the first node by convention
 * @author Pierre Schaus
 */
class Path(succ: Array[CPIntVar], start: CPIntVar, end: CPIntVar, length: CPIntVar) extends Constraint(succ(0).store, "Path") {

  // for each node, it's position in the path
  val y = Array.fill(succ.size)(CPIntVar(0 until succ.size)(s))
  

  override def setup(l: CPPropagStrength): CPOutcome = {    
    
    for (v <- 0 until succ.size; u <- 0 until succ.size; if (u != v)) {
      // succ(v) == u => succ(u) != u if u != end
      if (s.post((((succ(v) ?!== u) or (succ(u) ?!== u)) === 1).when(end ?!== u)) == CPOutcome.Failure) return CPOutcome.Failure
    }

    if (s.post(elementVar(y,start,0)) == CPOutcome.Failure) return CPOutcome.Failure
    if (s.post(elementVar(y,end,length)) == CPOutcome.Failure) return CPOutcome.Failure
    
    for (v <- 0 until succ.size) {
       
       if (s.post((elementVar(y,succ(v),y(v)+1).when((succ(v) ?!== v) && (end ?!== v)))) == CPOutcome.Failure) return CPOutcome.Failure
    }
    if (s.post(allDifferent(succ),l) == CPOutcome.Failure) return CPOutcome.Failure
    
    return CPOutcome.Success
  }

}
