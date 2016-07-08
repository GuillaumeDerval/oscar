package oscar.modeling.typetest

abstract class TypeAdaptingBinaryOp[T <: Number](left: Expression[T], right: Expression[T])(implicit eval: TypeAdaptingBinaryOp.CanEvaluate[T, T])
  extends Expression[T] with Mappable[T, T] {
  def evaluate(data: Data) = eval(this, left.evaluate(data), right.evaluate(data))
  def subExpressions() = Seq(left, right)

  def evalInt(left: Int, right: Int): Int
  def evalDouble(left: Double, right: Double): Double
}

object TypeAdaptingBinaryOp {
  trait CanEvaluate[-In <: Number, +Out <: Number] {
    def apply(op: TypeAdaptingBinaryOp[_], left: In, right: In): Out
  }
  object CanEvaluate {
    import ValueConversions._
    implicit val intEval = new CanEvaluate[Integer, Integer] {
      def apply(op: TypeAdaptingBinaryOp[_], left: Integer, right: Integer) = op.evalInt(left, right)
    }
    implicit val doubleEval = new CanEvaluate[Number, Number] {
      def apply(op: TypeAdaptingBinaryOp[_], left: Number, right: Number) = op.evalDouble(left, right)
    }
  }
}