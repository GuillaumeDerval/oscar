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

import oscar.algo.search.Outcome
import oscar.cp.core._
import oscar.algo.search.Outcome._
import oscar.cp.core.variables.CPGraphVar

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 * Defines the constraint Symmetric(G)
 * 	this constraint holds if G is symmetric 
 */

class GraphSymmetric(val g : CPGraphVar) extends Constraint(g.s, "Symmetric") {
  
	override def setup(l: CPPropagStrength): Outcome = {
	  // add filter when domain changes
	  g.callPropagateWhenDomainChanges(this)
	  propagate()
	}
	
	override def propagate(): Outcome = {
	  // if an edge e is possible
	  //  	if there is no opposite, the edge should be removed
	  // 	if the edge is required, the opposite should be required
	  for (n <- g.possibleNodes){ 
	    for (e <- g.possibleEdges(n)){
	      val (src, dest) = g.edge(e)
	      if (g.possibleEdges(dest).map(g.edge(_)).contains(g.edge(e))){
	        // there is an opposite, check for required (only one side, the other will be done later)
	        if (g.requiredEdges(n).contains(e)){
	        	// as e is required, the opposite should be required
	        	if (g.addEdgeToGraph(dest, src) == Failure) return Failure
	        }
	      } else { 
	        // no possible opposite arc
	        // remove the edge (will fail if the edge was required)
	        if (g.removeEdgeFromGraph(src, dest) == Failure) return Failure
	      }
	    }
	  }
	  
	  Suspend
	}
}