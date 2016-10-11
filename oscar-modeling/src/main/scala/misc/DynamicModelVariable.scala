package misc

import java.io.{IOException, ObjectInputStream, ObjectOutputStream}

import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, KryoSerializable}
import models.Model

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
