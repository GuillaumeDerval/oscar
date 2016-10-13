package oscar.modeling.algebra

import oscar.modeling.misc.VariableNotBoundException

/**
 * or_i a_i = output
 */
case class Or(a: Array[BoolExpression]) extends BoolExpression {
  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  override def evaluateBool(): Boolean = a.exists(_.evaluateBool())

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = a

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new Or(a.map(func(_).asInstanceOf[BoolExpression]))
}

object Or {
  def apply(a: BoolExpression*): Or = new Or(a.toArray)
}