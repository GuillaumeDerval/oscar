package oscar.modeling.typetest

class WeightedSum[T <: Number](vars: Array[Expression[T]], coeffs: Array[ConstantLike[T]])(implicit eval: WeightedSum.CanEvaluate[T, T]) extends Expression[Number] {
  def evaluate(data: Data) = eval(vars.map(_.evaluate(data)), coeffs.map(_.evaluate(data)))
  def subExpressions() = vars ++ coeffs
}

object WeightedSum {
  trait CanEvaluate[-In <: Number, +Out <: Number] {
    def apply(vars: Seq[In], coeffs: Seq[In]): Out
  }
  object CanEvaluate {
    import ValueConversions._
    implicit val intEval = new CanEvaluate[Integer, Integer] {
      def apply(vars: Seq[Integer], coeffs: Seq[Integer]) =
        vars.zip(coeffs).foldLeft(0)((acc: Int, p: (Integer, Integer)) => acc + p._1*p._2)
    }
    implicit val doubleEval = new CanEvaluate[Number, Number] {
      def apply(vars: Seq[Number], coeffs: Seq[Number]) =
        vars.zip(coeffs).foldLeft(0.0)((acc: Double, p: (Number, Number)) => acc + p._1*p._2)
    }
  }
}