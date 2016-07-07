package oscar.modeling.typetest

trait Var[+T <: ValueType] extends Expression[T] {
  def subExpressions() = Seq()
  def evaluate(data: Data) = {
    if (data.hasValue(this))
      data.getValue(this)
    else
      throw new UnsupportedOperationException("No value defined for this variable")
  }

  def isBound: Boolean
  def value: T
}

class Parameter[T <: ValueType] extends Var[T] {
  def isBound = false
  def value = throw new UnsupportedOperationException("Parameters do not have values")
}

trait Data {
  def hasValue[T <: ValueType](v: Var[T]): Boolean
  def getValue[T <: ValueType](v: Var[T]): T
}