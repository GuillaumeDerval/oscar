package oscar.modeling.typetest

case class BinarySum[T <: Number](left: Expression[T], right: Expression[T])(implicit eval: TypeAdaptingBinaryOp.CanEvaluate[T, T])
  extends TypeAdaptingBinaryOp[T](left, right) {

  def evalInt(left: Int, right: Int) = left + right
  def evalDouble(left: Double, right: Double) = left + right

  def mapSubExpressions[V >: T <: T](func: Expression[T] => Expression[V]) =
    BinarySum(func(left), func(right))
}