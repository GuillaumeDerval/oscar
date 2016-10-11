package algebra

import misc.VariableNotBoundException

/**
 * a & b
 */
case class BinaryAnd(a: BoolExpression, b: BoolExpression) extends BoolExpression {
  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  override def evaluateBool(): Boolean = a.evaluateBool() && b.evaluateBool()

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = Array(a,b)

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new BinaryAnd(func(a).asInstanceOf[BoolExpression], func(b).asInstanceOf[BoolExpression])
}