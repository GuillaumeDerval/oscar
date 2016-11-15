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

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class AllDiffAC extends Constraint {

	private CPIntVar[] x;

	public AllDiffAC(CPIntVar[] x) {
		super(x[0].store(), "Alldifferent AC");
		this.x = x;
	}

	@Override
	public Outcome setup(CPPropagStrength l) {
		CPIntVar nvalues = CPIntVar.apply(s(), x.length);
		Outcome ok = s().post(new AtLeastNValueAC(x, nvalues));
		if (ok == Outcome.Failure) {
			return Outcome.Failure;
		} else {
			return Outcome.Success;
		}
	}
}
