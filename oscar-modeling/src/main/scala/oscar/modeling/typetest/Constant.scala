package oscar.modeling.typetest

trait ConstantLike[+T <: ValueType] extends Expression[T]

case class Constant[T <: ValueType](value: T) extends ConstantLike[T] {
  def evaluate(data: Data) = value
  def subExpressions() = Seq()
}
object Constant {
  def apply(d: Double) = new Constant(new Number(d))
  def apply(i: Int) = new Constant(new Integer(i))
}

case class ParameterExpression[T <: ValueType](expr: Expression[T]) extends ConstantLike[T] {
  def evaluate(data: Data) = expr.evaluate(data)
  def subExpressions() = expr.subExpressions()
}
