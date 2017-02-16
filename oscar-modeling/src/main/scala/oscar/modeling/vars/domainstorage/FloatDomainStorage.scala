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

package oscar.modeling.vars.domainstorage

import oscar.algo.vars.FloatVarLike
import oscar.modeling.misc.VariableNotBoundException
import oscar.modeling.vars.FloatVarImplem

class FloatDomainStorage(val min: Double, val max: Double, val name: String) extends DomainStorage with FloatVarImplem with FloatVarLike
{
  override def context = throw new Exception("This variable is not instantiated and thus has no context")

  /**
    * @return true if the domain of the variable has exactly one value,
    *         false if the domain has more than one value
    */
  override def isBound: Boolean = min == max

  /**
    * Test if a value is in the domain
    *
    * @param value : value to test
    * @return true if the domain contains the value val, false otherwise
    */
  override def hasValue(value: Double): Boolean = value >= min && value <= max

  /**
    * @return returns the set this variable represents, if it is bound
    */
  override def value(): Double = if(isBound) max else throw new VariableNotBoundException()
}

