package oscar.modeling.algebra.floating

object Dist {
  /**
    * Computes the distance between 'a' and 'b': Abs(a-b)
    */
  def apply(a: FloatExpression, b: FloatExpression): FloatExpression = Abs(a-b)
}