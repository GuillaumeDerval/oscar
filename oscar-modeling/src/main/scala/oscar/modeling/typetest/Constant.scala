package oscar.modeling.typetest

case class Constant[T <: ValueType](value: T) extends Expression[T] {
  override def evaluate() = value
  override def subExpressions() = Seq()
}
