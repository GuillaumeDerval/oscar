package oscar.modeling.typetest

trait ConstantLike[T <: ValueType] extends Expression[T]

case class Constant[T <: ValueType](value: T) extends ConstantLike[T] {
  override def evaluate() = value
  override def subExpressions() = Seq()
}

class Parameter[T <: ValueType](expr: Expression[T]) extends ConstantLike[T] {
  def evaluate() = expr.evaluate()
  def subExpressions() = expr.subExpressions()
}
