package oscar.modeling.typetest

case class BinarySum[T <: Number](left: Expression[T], right: Expression[T])
  extends BinaryNumberOp[T](left, right) with Mappable[T, Number] with Derivable[T] {

  protected def evaluateInt(l: Int, r: Int) = l+r
  protected def evaluateDouble(l: Double, r: Double) = l+r

  def mapSubExpressions[V >: T <: Number](func: (Expression[T]) => Expression[V]): BinarySum[V] = BinarySum(func(left), func(right))

  def derive() = (left, right) match {
    case (l: Derivable[T], r: Derivable[T]) => BinarySum(l.derive(), r.derive())
    case _ => throw new UnsupportedOperationException("Derivation not supported in sub-expressions: " + left + " " + right)
  }
}
