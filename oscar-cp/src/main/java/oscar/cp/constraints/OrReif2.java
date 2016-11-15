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

import oscar.algo.reversible.ReversibleBoolean;
import oscar.algo.reversible.ReversibleInt;
import oscar.algo.search.Outcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.variables.CPBoolVar;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;

/**
 * Logical Or Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class OrReif2 extends Constraint {
	
	private CPBoolVar [] x;
	private CPBoolVar y;
	
	ReversibleInt nbBound;
	ReversibleBoolean ytrue;
	


    /**
     * y is true if at least one of the xi's is true, false otherwise
     * @param x
     * @param y
     */
	public OrReif2(CPBoolVar [] x, CPBoolVar y) {
		super(x[0].store(),"Or");
		this.x = x;
		this.y = y;

	}

	@Override
	public Outcome setup(CPPropagStrength l) {
	    if (x.length == 2) {
	        if (s().post(new BinaryOr(x[0],x[1],y)) == Outcome.Failure) return Outcome.Failure;
	        else return Outcome.Success;
	    }
		nbBound = new ReversibleInt(s(),0); // number of values assigned to false
		ytrue = new ReversibleBoolean(s(),false);
		for (int i = 0; i < x.length; i++) {
			if (x[i].isTrue()) {
				if (y.assign(1) == Outcome.Failure) {
					return Outcome.Failure;
				} else {
					return Outcome.Success;
				}
			}
		}
		// we know no variables from X are bound to 1
		for (int i = 0; i < x.length; i++) {
			if (!x[i].isBound()) {
				x[i].callValBindIdxWhenBind(this, i);
			} else {
				assert(x[i].isFalse());
				nbBound.incr();
			}
		}
		if (!y.isBound()) {
			if (nbBound.getValue() == x.length) {
				if (y.assign(0) == Outcome.Failure) {
					return Outcome.Failure;
				}
			}
			y.callValBindWhenBind(this);
		}
		else {
			if (y.min() == 0) {
				for (int i = 0; i < x.length; i++) {
						if (x[i].assign(0) == Outcome.Failure) {
							return Outcome.Failure;
						}
				}
				return Outcome.Success;
			} else { // y = true
				ytrue.setValue(true);
				if (nbBound.getValue() == x.length-1) { // only one is not bound to false, this one must be set to true
					for (int i = 0; i < x.length; i++) {
						if (!x[i].isBound()) {
							if (x[i].assign(1) == Outcome.Failure) {
								return Outcome.Failure;
							}
							return Outcome.Success;
						}
					}
				}
			}
		}
		return Outcome.Suspend;
	}
	
	
	@Override
	public Outcome valBindIdx(CPIntVar var, int idx) {
		if (var.min() == 1) {
			if (y.assign(1) == Outcome.Failure) {
				return Outcome.Failure;
			}
			return Outcome.Success;
		} else {
			nbBound.incr();			
			if (nbBound.getValue() == x.length) {
				if (y.assign(0) == Outcome.Failure) {
					return Outcome.Failure;
				}
			} else if (nbBound.getValue() == x.length-1 && ytrue.getValue()){
				for (int i = 0; i < x.length; i++) {
					if (!x[i].isBound()) {
						if (x[i].assign(1) == Outcome.Failure) {
							return Outcome.Failure;
						}
						return Outcome.Success;
					}
				}
				return Outcome.Suspend;
			}
		}
		return Outcome.Suspend;
	}
	
	@Override
	public Outcome valBind(CPIntVar yvar) {
		if (yvar.min() == 0) {
			for (int i = 0; i < x.length; i++) {
					if (x[i].assign(0) == Outcome.Failure) {
						return Outcome.Failure;
					}
				
			}
			return Outcome.Success;
		} else {
			ytrue.setValue(true);
			if (nbBound.getValue() == x.length-1){
				for (int i = 0; i < x.length; i++) {
					if (!x[i].isBound()) {
						if (x[i].assign(1) == Outcome.Failure) {
							return Outcome.Failure;
						}
						return Outcome.Success;
					}
				}
			}
		}
		return Outcome.Suspend;
	}

}
