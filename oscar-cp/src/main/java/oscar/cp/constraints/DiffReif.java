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
import oscar.cp.core.CPStore;

/**
 * Reified constraint.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class DiffReif extends Constraint {

	CPIntVar x;
	int v;
	CPBoolVar b;
	

	/**
     * Ask that x and v take different values if and only if b is true. <br>
     * (x == v) <=> b
     * @param x
     * @param v
     */
	public DiffReif(CPIntVar x, int v, CPBoolVar b) {
		super(x.store(),"DiffReif");
		this.x = x;
		this.v = v;
		this.b = b;
	}
	
	@Override
	public Outcome setup(CPPropagStrength l) {
		priorityBindL1_$eq(CPStore.MAXPRIORL1());
		priorityRemoveL1_$eq(CPStore.MAXPRIORL1());
		
		if (x.isBound() || b.isBound())
			return valBind(x);
		else if (b.isBound())
			return valBind(b);
		else {
			x.callValBindWhenBind(this);
			b.callValBindWhenBind(this);
			//x.addAC5Bounds(this);
			x.callValRemoveWhenValueIsRemoved(this);
			return Outcome.Suspend;
		}
	}
	
	@Override
	public Outcome updateBounds(CPIntVar x) {
		if (x.getMax() < v || x.getMin() > v) {
			if (b.assign(1) == Outcome.Failure) {
				return Outcome.Failure;
			}
			return Outcome.Success;
		}
		return Outcome.Suspend;
	}
	

	@Override
	public Outcome valRemove(CPIntVar x, int val) {
		if (val == v) {
			if (b.assign(1) == Outcome.Failure) {
				return Outcome.Failure;
			}
			return Outcome.Success;
		}
		return Outcome.Suspend;
	}
	

	@Override
	public Outcome valBind(CPIntVar var) {
		if (b.isBound()) {
			if (b.min() == 1) {
				//x != v
				if (x.removeValue(v) == Outcome.Failure) {
					return Outcome.Failure;
				}
			} else {
				//x == v
				if (x.assign(v) == Outcome.Failure) {
					return Outcome.Failure;
				}				
			}
			return Outcome.Success;
		}
		
		if (x.isBound()) {
			if (x.min() == v) {
				if (b.assign(0) == Outcome.Failure) {
					return Outcome.Failure;
				}
			}
			else {
				if (b.assign(1) == Outcome.Failure) {
					return Outcome.Failure;
				}
			}
			return Outcome.Success;
		}
		
		return Outcome.Suspend;
	}

}

