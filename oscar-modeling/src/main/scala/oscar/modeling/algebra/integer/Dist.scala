package oscar.modeling.algebra.integer

object Dist {
  /**
    * Computes the distance between 'a' and 'b': Abs(a-b)
    */
  def apply(a: IntExpression, b: IntExpression): IntExpression = Abs(a-b)
}