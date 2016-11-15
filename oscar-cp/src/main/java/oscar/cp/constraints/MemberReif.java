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
import oscar.algo.reversible.SparseSet;
import oscar.algo.search.Outcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPBoolVar;
import oscar.cp.core.variables.CPIntVar;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class MemberReif extends Constraint {

	private CPIntVar x;
	private SparseSet set;
	private CPBoolVar b;
	private ReversibleInt inter;  // size of the intersection of D(x) and set
    private ReversibleInt xsize; // size of x

	/**
	 * Reified constraint: b is true iff x is a value from the set
	 * @param x
	 * @param set, should not be modified externally after posting the constraint
	 * @param b
	 */
	public MemberReif(CPIntVar x, SparseSet set, CPBoolVar b) {
		super(x.store(),"MemberReif");
		this.x = x;
		this.set = set;
		this.b = b;
	}

	@Override
	public Outcome setup(CPPropagStrength l) {
		if (b.isBound()) {
            return valBind(b);
        }
        if (x.isBound()) {
            return valBind(x);
        }

        int interSize = 0;
		for (int v : set) {
			if (x.hasValue(v)) interSize++;
		}

		if (interSize == 0) { // intersection between the domain of x and set is empty
			return emptyIntersection();
		}
        if (interSize >= x.getSize()) { // D(x) is included in set
           return fullIntersection();
        }
        inter = new ReversibleInt(s(),interSize);
        xsize = new ReversibleInt(s(),x.getSize());

		x.callValBindWhenBind(this);
        b.callValBindWhenBind(this);
        x.callValRemoveWhenValueIsRemoved(this);

		return Outcome.Suspend;

	}

    @Override
    public Outcome valRemove(CPIntVar var, int val) {
        xsize.decr();
        if (set.hasValue(val)) {
            inter.decr();
        }
        if (inter.getValue() == 0) {
            return emptyIntersection();
        }
        if (inter.getValue() == xsize.getValue()) { // D(x) is included in set
           return fullIntersection();
        }
        return  Outcome.Suspend;
    }

    @Override
    public Outcome valBind(CPIntVar var) {
        assert(var.isBound());
		if (var == x) {
             if (set.hasValue(x.min())) {
                 if (b.assign(1) == Outcome.Failure) {
                     return Outcome.Failure;
                 }
             } else {
                 if (b.assign(0) == Outcome.Failure) {
                     return Outcome.Failure;
                 }
             }
             return Outcome.Success;
        } else {
            assert(var == b);
            if (b.min() == 1) { // x must be a member of the set
                return removeValues(false); // remove all non member values of x and return success if no failure
            } else { // x cannot be a member of the set
                return removeValues(true); // remove all member values of x and return success if no failure
            }
        }
	}

    private Outcome emptyIntersection() {
		if (b.assign(0) == Outcome.Failure) {
			return Outcome.Failure;
		}
		return Outcome.Success;
	}

    private Outcome fullIntersection() {
		if (b.assign(1) == Outcome.Failure) {
			return Outcome.Failure;
		}
		return Outcome.Success;
	}

    /**
     * @param memberValue = true then remove all values from x that are also member of set
     *        memberValue = false then remove all values from x that are not member of set
     * @return Failure if the domain of x becomes empty, Success otherwise
     */
    private Outcome removeValues(boolean memberValue) {
        assert(b.isBound());
        for (int val = x.min(); val <= x.max(); val++) {
        	if (x.hasValue(val)) {
        		if ((memberValue && set.hasValue(val)) || (!memberValue && !set.hasValue(val))) {
        			if (x.removeValue(val) == Outcome.Failure) {
        				return Outcome.Failure;
        			}
        		}
        	}
        }
		return Outcome.Success;
	}
}

