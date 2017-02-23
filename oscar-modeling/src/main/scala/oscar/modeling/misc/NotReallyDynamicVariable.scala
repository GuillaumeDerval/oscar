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

package oscar.modeling.misc

import scala.util.DynamicVariable

/**
 * A version of DynamicVariable for when we do not need to be dynamic
 * @param init: the initial value to be set
 * @tparam T: the type of the values to store
 */
class NotReallyDynamicVariable[T](init: T = null) extends DynamicVariable(init) {
  var content = init

  override def withValue[S](newval: T)(thunk: => S): S = {
    val oldval = value
    content = newval

    try thunk
    finally content = oldval
  }

  override def value: T = content

  override def value_=(newval: T) = {
    content = newval
  }
}