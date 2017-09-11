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

package oscar.cbls.modeling

import oscar.cbls.lib.invariant.logic.LogicInvariants
import oscar.cbls.lib.invariant.minmax.MinMaxInvariants
import oscar.cbls.lib.invariant.numeric.NumericInvariants
import oscar.cbls.lib.invariant.seq.SeqInvariants
import oscar.cbls.lib.invariant.set.SetInvariants


trait Invariants
  extends LogicInvariants
  with MinMaxInvariants
  with NumericInvariants
  with SetInvariants
  with SeqInvariants
//TODO: routing seq invariants