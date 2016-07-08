package oscar.modeling.typetest

trait Var[+T <: ValueType] extends Expression[T] {
  def subExpressions() = Seq()
  def evaluate(data: Data) = {
    if (data.hasValue(this))
      data.getValue(this)
    else
      throw new UnsupportedOperationException("No value defined for this variable")
  }

  val name: String
  def isBound: Boolean
  def value: T
}

// Stub
class DoubleVar(val name: String, val lb: Double, val ub: Double) extends Var[Number] {
  var accessor: (=> Double) = lb
  var id: Int = -1

  def isBound = lb == ub
  def value = new Number(accessor)
}

class Parameter[T <: ValueType](val name: String) extends Var[T] {
  def isBound = false
  def value = throw new UnsupportedOperationException("Parameters do not have values")
}