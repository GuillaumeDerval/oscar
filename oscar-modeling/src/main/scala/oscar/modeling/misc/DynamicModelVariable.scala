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

import java.io.{IOException, ObjectInputStream, ObjectOutputStream}

import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, KryoSerializable}
import oscar.modeling.models.Model

import scala.util.DynamicVariable


@SerialVersionUID(13l)
class DynamicModelVariable() extends Serializable with KryoSerializable {
  private var v = new DynamicVariable[Model](null)

  def value: Model = v.value

  def withValue[S](newval: Model)(thunk: => S): S = {
    v.withValue(newval)(thunk)
  }

  def value_=(newval: Model) = {
    v.value = newval
  }

  override def toString: String = "DynamicModelVariable(" + value + ")"

  @throws(classOf[IOException])
  private def writeObject(out: ObjectOutputStream): Unit = {
    out.writeObject(this.value)
  }

  @throws(classOf[IOException])
  private def readObject(in: ObjectInputStream): Unit = {
    v = new DynamicVariable[Model](in.readObject().asInstanceOf[Model])
  }

  override def write(kryo: Kryo, output: Output): Unit = {
    kryo.writeClassAndObject(output, this.value)
  }

  override def read(kryo: Kryo, input: Input): Unit = {
    v = new DynamicVariable[Model](kryo.readClassAndObject(input).asInstanceOf[Model])
  }
}
