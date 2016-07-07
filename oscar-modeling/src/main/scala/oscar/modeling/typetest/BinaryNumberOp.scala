package oscar.modeling.typetest

abstract class BinaryNumberOp[T <: Number](left: Expression[T], right: Expression[T]) extends Expression[T] {
  def evaluate(data: Data): T =
    ((left.evaluate(data), right.evaluate(data)) match {
      case (l: Integer, r: Integer) => new Integer(evaluateInt(l.asInt, r.asInt))
      case (l, r) => new Number(evaluateDouble(l.asDouble, r.asDouble))
    }).asInstanceOf[T]

  def subExpressions() = Seq(left, right)

  protected def evaluateInt(l: Int, r: Int): Int
  protected def evaluateDouble(l: Double, r:Double): Double
}
