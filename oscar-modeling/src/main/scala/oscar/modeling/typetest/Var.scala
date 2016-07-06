package oscar.modeling.typetest

trait Var[+T <: ValueType] extends Expression[T] {
  def subExpressions() = Seq()
}