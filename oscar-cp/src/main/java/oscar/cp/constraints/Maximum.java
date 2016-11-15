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

import oscar.algo.reversible.ReversibleInt;
import oscar.algo.search.Outcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Maximum extends Constraint {
	
	
	private CPIntVar [] x;
	private CPIntVar y;
	private ReversibleInt maxval;
	private ReversibleInt maxvalsupport;
	
	private ReversibleInt minval;
	private ReversibleInt minvalsupport;
	
	/**
	 * Constraint y = max(x)
	 * @param x
	 * @param y
	 */
	public Maximum(CPIntVar [] x, CPIntVar y) {
		super(x[0].store(),"Maximum");
		this.x = x;
		this.y = y;
		maxval = new ReversibleInt(s(), 0);
		maxvalsupport = new ReversibleInt(s(), 0);
		minval = new ReversibleInt(s(), 0);
		minvalsupport = new ReversibleInt(s(), 0);
	}
	
	private void updateSupport() {
		int min = Integer.MIN_VALUE;
		int max = Integer.MIN_VALUE;
		for (int i = 0; i < x.length; i++) {
			int m = x[i].getMin();
			int M = x[i].getMax();
			
			if (m > min) {
				minvalsupport.setValue(i);
				minval.setValue(m);
				min = m;
			}
			if (M > max) {
				maxvalsupport.setValue(i);
				maxval.setValue(M);
				max = M;
			}
		}
	}

	@Override
	public Outcome setup(CPPropagStrength l) {
		
		for (int i=0; i < x.length; i++) {			
			if (x[i].updateMax(y.getMax()) == Outcome.Failure) {
				return Outcome.Failure;
			}
		}
		updateSupport();
		if (y.updateMin(minval.getValue()) == Outcome.Failure) {
			return Outcome.Failure;
		}
		if (y.updateMax(maxval.getValue()) == Outcome.Failure) {
			return Outcome.Failure;
		}
		
		for (int i = 0; i < x.length; i++) {
			if (!x[i].isBound() && (x[i].getMax() > y.getMin())) {
				x[i].callUpdateBoundsIdxWhenBoundsChange(this, i);
			}
		}
		if (!y.isBound()) {
			y.callUpdateBoundsWhenBoundsChange(this);
		}	
		return Outcome.Suspend;
	}
	
	@Override
	public Outcome updateBoundsIdx(CPIntVar x, int idx) {
		if (idx == minvalsupport.getValue() || idx == maxvalsupport.getValue()) {
			updateSupport();
			if (y.updateMin(minval.getValue()) == Outcome.Failure) {
				return Outcome.Failure;
			}
			if (y.updateMax(maxval.getValue()) == Outcome.Failure) {
				return Outcome.Failure;
			}
		}
		if (x.isBound() && x.min() == maxval.getValue()) {
			if (y.assign(maxval.getValue()) == Outcome.Failure) {
				return Outcome.Failure;
			}
			return Outcome.Success;
		}
		
		return Outcome.Suspend;
	}
	
	
	@Override
	public Outcome updateBounds(CPIntVar y) {
		for (int i=0; i < x.length; i++) {			
			if (x[i].updateMax(y.getMax()) == Outcome.Failure) {
				return Outcome.Failure;
			}
		}
		return Outcome.Suspend;
	}

}
