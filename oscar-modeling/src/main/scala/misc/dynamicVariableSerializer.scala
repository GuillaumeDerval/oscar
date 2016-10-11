package misc

import akka.serialization._
import models.Model

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