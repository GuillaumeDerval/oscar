package oscar.modeling.typetest

case class BinarySum[T <: Number](left: Expression[T], right: Expression[T])(implicit eval: CanSum[T, T])
  extends Expression[T] with Mappable[T, T] {
  def evaluate(data: Data) = eval(left.evaluate(data), right.evaluate(data))
  def subExpressions() = Seq(left, right)

  def mapSubExpressions[V >: T <: T](func: Expression[T] => Expression[V]) =
    BinarySum(func(left), func(right))
}

trait CanSum[-T <: Number, +U <: Number] {
  def apply(left: T, right: T): U
}
object CanSum {
  import ValueConversions._
  implicit val intSum = new CanSum[Integer, Integer] {
    def apply(left: Integer, right: Integer) = left + right
  }
  implicit val doubleSum = new CanSum[Number, Number] {
    def apply(left: Number, right: Number) = left + right
  }
}