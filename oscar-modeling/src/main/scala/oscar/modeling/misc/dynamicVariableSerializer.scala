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

import akka.serialization._
import oscar.modeling.models.Model

import scala.util.DynamicVariable

class dynamicVariableSerializer extends Serializer {
  def includeManifest: Boolean = false

  def identifier = 427892370

  def toBinary(obj: AnyRef): Array[Byte] = {
    assert(obj.isInstanceOf[scala.util.DynamicVariable[Model]])
    val model = obj.asInstanceOf[scala.util.DynamicVariable[Model]].value
    new akka.serialization.JavaSerializer().toBinary(model)
  }

  def fromBinary(bytes: Array[Byte], clazz: Option[Class[_]]): AnyRef = {
    val model = new akka.serialization.JavaSerializer().fromBinary(bytes).asInstanceOf[Model]
    new DynamicVariable[Model](model)
  }
}