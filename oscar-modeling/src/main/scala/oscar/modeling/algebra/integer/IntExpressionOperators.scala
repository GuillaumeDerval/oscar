package oscar.modeling.algebra.integer

import oscar.modeling.algebra.bool.{And, BoolExpression, Or}

/**
 * Basic operations on expressions
 */
trait IntExpressionOperators {
  def abs(a: IntExpression): IntExpression = Abs(a)
  def min(a: IntExpression*): IntExpression = Min(a.toArray)
  def min(a: Array[IntExpression]): IntExpression = Min(a)
  def max(a: IntExpression*): IntExpression = Max(a.toArray)
  def max(a: Array[IntExpression]): IntExpression = Max(a)
  def sum(a: IntExpression*): IntExpression = Sum(a.toArray)
  def sum(a: Array[IntExpression]): IntExpression = Sum(a)
  def count(X: Array[IntExpression], Y: IntExpression) = Count(X, Y)
}
