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

package oscar.modeling.vars.nostorage

import oscar.algo.vars.FloatVarLike
import oscar.modeling.vars.FloatVarImplem
import oscar.modeling.vars.domainstorage.DomainStorage

class NoFloatDomainStorage extends DomainStorage with FloatVarImplem with FloatVarLike
{
  fail
  def fail = throw new RuntimeException("This is a fake storage, and should'nt be instantiated")
  override def context = fail
  override def isBound: Boolean = fail
  override def hasValue(value: Double): Boolean = fail
  override def value(): Double = fail
  override def min: Double = fail
  override def max: Double = fail
  override def name: String = fail
}

