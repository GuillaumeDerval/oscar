package oscar.modeling.algebra.bool

/**
  * Basic operations on expressions
  */
trait BoolExpressionOperators {
  def or(a: BoolExpression*): BoolExpression = Or(a.toArray)
  def or(a: Array[BoolExpression]): BoolExpression = Or(a)
  def and(a: BoolExpression*): BoolExpression = And(a.toArray)
  def and(a: Array[BoolExpression]): BoolExpression = And(a)
}
