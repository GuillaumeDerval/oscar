package oscar.modeling.algebra.floating

/**
 * Basic operations on expressions
 */
trait FloatExpressionOperators {
  def abs(a: FloatExpression): FloatExpression = Abs(a)
  def min(a: FloatExpression*): FloatExpression = Min(a.toArray)
  def min(a: Array[FloatExpression]): FloatExpression = Min(a)
  def max(a: FloatExpression*): FloatExpression = Max(a.toArray)
  def max(a: Array[FloatExpression]): FloatExpression = Max(a)
  def sum(a: FloatExpression*): FloatExpression = Sum(a.toArray)
  def sum(a: Array[FloatExpression]): FloatExpression = Sum(a)
}