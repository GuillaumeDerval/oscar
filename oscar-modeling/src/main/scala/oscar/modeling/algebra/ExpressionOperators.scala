package oscar.modeling.algebra

/**
 * Basic operations on expressions
 */
object ExpressionOperators {
  def abs(a: IntExpression): IntExpression = new Abs(a)
  def min(a: IntExpression*): IntExpression = new Min(a.toArray)
  def min(a: Array[IntExpression]): IntExpression = new Min(a)
  def max(a: IntExpression*): IntExpression = new Max(a.toArray)
  def max(a: Array[IntExpression]): IntExpression = new Max(a)
  def sum(a: IntExpression*): IntExpression = new Sum(a.toArray)
  def sum(a: Array[IntExpression]): IntExpression = new Sum(a)
  def or(a: BoolExpression*): BoolExpression = new Or(a.toArray)
  def or(a: Array[BoolExpression]): BoolExpression = new Or(a)
  def and(a: BoolExpression*): BoolExpression = new And(a.toArray)
  def and(a: Array[BoolExpression]): BoolExpression = new And(a)
  def count(X: Array[IntExpression], Y: IntExpression) = new Count(X, Y)
}
