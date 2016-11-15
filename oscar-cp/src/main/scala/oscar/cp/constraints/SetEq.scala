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
import oscar.cp.core.variables.CPSetVar
import oscar.cp.core.delta.{DeltaSetVar, PropagatorSetVar}

/**
 * Implementation of Equality constraint for CPSetVar
 * @author Leonard Debroux leonard.debroux@gmail.com
 */
class SetEq(val a: CPSetVar, val b: CPSetVar) extends Constraint(a.store, "SetEq") {
	override def setup(l: CPPropagStrength): Outcome = {
	  
	  def filterB(d: DeltaSetVar): Outcome = {
	    for (v <- d.deltaRequired) {
	      if(b.requires(v) == Failure) return Failure
	    }
	    for (v <- d.deltaPossible) {
	      if(b.excludes(v) == Failure) return Failure
	    }
	    Suspend
	  }
	  def filterA(d: DeltaSetVar): Outcome = {
	    for (v <- d.deltaRequired) {
	      if(a.requires(v) == Failure) return Failure
	    }
	    for (v <- d.deltaPossible) {
	      if(a.excludes(v) == Failure) return Failure
	    }
	    Suspend
	  }
	  
	  a.filterWhenDomainChangesWithDelta() { d =>
	    filterB(d)
	  }
	  b.filterWhenDomainChangesWithDelta() { d =>
	    filterA(d)
	  }
	  
	  for(v <- a.requiredValues) {
	    if(b.requires(v) == Failure) return Failure
	  }
	  for(v <- b.requiredValues) {
	    if(a.requires(v) == Failure) return Failure
	  }
	  
	  for(v <- a.possibleNotRequiredValues) {
	    if(!(b.possibleNotRequiredValues contains v)){
	      if(a.excludes(v) == Failure) return Failure
	    }
	  }
	  for(v <- b.possibleNotRequiredValues) {
	    if(!(a.possibleNotRequiredValues contains v)){
	      if(b.excludes(v) == Failure) return Failure
	    }
	  }
	  
	  Success
	}
}
