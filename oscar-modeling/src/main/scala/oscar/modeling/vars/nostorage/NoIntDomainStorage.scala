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

import oscar.algo.vars.IntVarLike
import oscar.modeling.vars.IntVarImplem
import oscar.modeling.vars.domainstorage.DomainStorage

import scala.util.Random

class NoIntDomainStorage extends DomainStorage with IntVarImplem with IntVarLike
{
  fail
  def fail = throw new RuntimeException("This is a fake storage, and should'nt be instantiated")
  override def isContinuous: Boolean = fail
  override def context = fail
  override def isBound: Boolean = fail
  override def isBoundTo(v: Int): Boolean = fail
  override def hasValue(value: Int): Boolean = fail
  override def valueAfter(value: Int): Int = fail
  override def valueBefore(value: Int): Int = fail
  override def randomValue(rand: Random): Int = fail
  override def min: Int = fail
  override def max: Int = fail
  override def iterator: Iterator[Int] = fail
  override def toArray: Array[Int] = fail
  override def name: String = fail
}

