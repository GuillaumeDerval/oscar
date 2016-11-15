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

import oscar.algo.search.Outcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.variables.CPBoolVar;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;

/**
 * Reified constraint.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class DiffReifVar extends Constraint {

	CPIntVar x;
	CPIntVar y;
	CPBoolVar b;
	

	/**
     * Ask that x and v take different values if and only if b is true. <br>
     * (x != y) <=> b
     * @param x
     * @param y
     */
	public DiffReifVar(CPIntVar x, CPIntVar y, CPBoolVar b) {
		super(x.store(),"DiffReif");
		this.x = x;
		this.y = y;
		this.b = b;
	}
	
	@Override
	public Outcome setup(CPPropagStrength l) {
		if (b.isBound()) {
			return valBind(b);
		} 
		else if (x.isBound()) {
			return valBind(x);
		} 
		else if (y.isBound()) {
			return valBind(y);
		}
		else {
			x.callPropagateWhenDomainChanges(this);
			y.callPropagateWhenDomainChanges(this);	
			b.callValBindWhenBind(this);
			x.callValBindWhenBind(this);
			y.callValBindWhenBind(this);
			return propagate();
		}
	}
	
	@Override
	public Outcome valBind(CPIntVar var) {
		if (b.isBound()) {
			if (b.min() == 1) {
				// x != y
				if (s().post(new DiffVar(x,y)) == Outcome.Failure) {
					return Outcome.Failure;
				}
			} else {
				//x == y
				if (s().post(new Eq(x,y))  == Outcome.Failure) {
					return Outcome.Failure;
				}
			}
			return Outcome.Success;
		}	
		else if (x.isBound()) {
			if (s().post(new DiffReif(y,x.min(),b)) == Outcome.Failure) {
				return Outcome.Failure;
			}
			return Outcome.Success;
		}
		else if (y.isBound()) {
			if (s().post(new DiffReif(x,y.min(),b)) == Outcome.Failure) {
				return Outcome.Failure;
			}
			return Outcome.Success;
		}
		return Outcome.Success;
	}
	
	
	
	@Override
	public Outcome propagate() {
		// if the domains of x and y are disjoint we can set b to false and return success
		if (x.getMax() < x.getMin()) {
			if (b.assign(1) == Outcome.Failure) {
				return Outcome.Failure;
			}
			return Outcome.Success;
		}
		else if (y.getMax() < x.getMin()) {
			if (b.assign(1) == Outcome.Failure) {
				return Outcome.Failure;
			}
			return Outcome.Success;
		}
		else {
			// there is an overlap between the domain ranges
			// if no values in this overlapping range are common, set b to false
			int start = Math.max(x.getMin(), y.getMin());
			int end = Math.min(x.getMax(), y.getMax());
			boolean commonValues = false;
			for (int i = start; i <= end; i++) {
 				if (x.hasValue(i) && y.hasValue(i)) {
					commonValues = true;
					break;
				}
			}
			if (!commonValues) {
				if (b.assign(1) == Outcome.Failure) {
					return Outcome.Failure;
				}
				return Outcome.Success;
			}
			return Outcome.Suspend;
		}
		
	}
	


}

