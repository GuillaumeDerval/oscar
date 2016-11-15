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
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;
import oscar.cp.util.NumberUtils;

/**
 * Multiplication Constraint x * y = c
 * @author Pierre Schaus pschaus@gmail.com
 */
public class MulCteRes extends Constraint {

	private CPIntVar x, y;
	private int c;

    /**
     * x * y == c
     * @param x
     * @param y
     * @param c
     * @see CPIntVar#mul(cp.core.CPIntVar)
     */
	public MulCteRes(CPIntVar x, CPIntVar y, int c) {
		super(x.store(),"MulCteRes x*y=c");
		this.x = x;
		this.y = y;
		this.c = c;
	}
	
	@Override
	public Outcome setup(CPPropagStrength l) {
		
		if (x == y) {
			if (s().post(new Square(x,CPIntVar.apply(s(),c,c))) == Outcome.Failure) {
				return Outcome.Failure;
			}
			return Outcome.Success;
		}
		
		if (c == 0 && x.hasValue(0) && y.hasValue(0)) {
			x.callPropagateWhenDomainChanges(this);
			y.callPropagateWhenDomainChanges(this);
		} else {
			x.callPropagateWhenBoundsChange(this);
			y.callPropagateWhenBoundsChange(this);
		}
		// propagate must be called after attaching events because this propagator may not reach fix-point it-self.
		Outcome ok = propagate();
		if (ok != Outcome.Suspend) {
			return ok;
		}
		
		return Outcome.Suspend;
	}
		
	@Override
	public Outcome propagate() {
		
		if (c != 0) {
			if (x.removeValue(0) == Outcome.Failure) {
				return Outcome.Failure;
			}
			if (y.removeValue(0) == Outcome.Failure) {
				return Outcome.Failure;
			}
		}
		if (x.isBound()) {
			if (s().post(new MulCte(y,x.min(),CPIntVar.apply(s(), c,c))) == Outcome.Failure) {
				return Outcome.Failure;
			}
			return Outcome.Success;
		} else if (y.isBound()) {
			if (s().post(new MulCte(x,y.min(),CPIntVar.apply(s(), c,c))) == Outcome.Failure) {
				return Outcome.Failure;
			}
			return Outcome.Success;
		} else {
			if (c == 0) {
				boolean xZero = x.hasValue(0);
				boolean yZero = y.hasValue(0);
				if (xZero || yZero) {
					if (xZero ^ yZero) {
						if (xZero) {
							x.assign(0);
						} else {
							y.assign(0);
						}
						return Outcome.Success;
					}
				} else return Outcome.Failure;
			} else { // c != 0
				if (propagateVar(x,y) == Outcome.Failure) {
					return Outcome.Failure;
				}
				if (propagateVar(y,x) == Outcome.Failure) {
					return Outcome.Failure;
				}
			}
			return Outcome.Suspend;
		}
	}
	
	/**
	 * Filter domain of z with w * z == c with c!=0
	 */
	private Outcome propagateVar(CPIntVar w , CPIntVar z) {
		int a = w.getMin();
		int b = w.getMax();
		
		assert (c != 0);
		assert(a < b);
		
		if (a > 0 || b < 0) {
			// [a,b] > 0 or [a,b] < 0
			if (z.updateMin(NumberUtils.minCeilDiv(c,a,b)) == Outcome.Failure) {
				return Outcome.Failure;
			}
			if (z.updateMax(NumberUtils.maxFloorDiv(c,a,b)) == Outcome.Failure) {
				return Outcome.Failure;
			}
			return Outcome.Suspend;
		} else if (a == 0) {
			int after0 = w.valueAfter(0);
			// a=0 ... after0 ... b
			if (z.updateMin(NumberUtils.minCeilDiv(c,after0,b)) == Outcome.Failure) {
				return Outcome.Failure;
			}
			if (z.updateMax(NumberUtils.maxFloorDiv(c,after0,b)) == Outcome.Failure) {
				return Outcome.Failure;
			}
			return Outcome.Suspend;
		} else if (b == 0) {
			int before0 = w.valueBefore(0);
			// a ... before0 ... b=0
			if (z.updateMin(NumberUtils.minCeilDiv(c,before0,a)) == Outcome.Failure) {
				return Outcome.Failure;
			}
			if (z.updateMax(NumberUtils.maxFloorDiv(c,before0,a)) == Outcome.Failure) {
				return Outcome.Failure;
			}
			return Outcome.Suspend;
		} else { // a ... 0 ... b
			int before0 = w.valueBefore(0);
			int after0 = w.valueAfter(0);
			if (z.updateMin(NumberUtils.minCeilDiv(c, a, before0, after0, b)) == Outcome.Failure) {
				return Outcome.Failure;
			}
			if (z.updateMax(NumberUtils.maxFloorDiv(c, a, before0, after0, b)) == Outcome.Failure) {
				return Outcome.Failure;
			}			
		}
		return Outcome.Suspend;
	}	
}



