package oscar.modeling.algebra.floating

import oscar.modeling.algebra.Expression
import oscar.modeling.misc.VariableNotBoundException

case class Abs(a: FloatExpression) extends FloatExpression {
  /**
    * Evaluate this expression. All variables referenced have to be bound.
    *
    * @throws VariableNotBoundException when a variable is not bound
    * @return the value of this expression
    */
  override def evaluate(): Double = math.abs(a.evaluate())

  /**
    * Return a lower bound for this expression
    */
  override def min: Double = {
    0 //todo can be improved
  }

  /**
    * Return a higher bound for this expression
    */
  override def max: Double = {
    if(math.abs(a.max) > math.abs(a.min)) math.abs(a.max)
    else math.abs(a.min)
  }

  /**
    * Returns an iterable that contains all sub-expressions of this expression
    */
  override def subexpressions(): Iterable[FloatExpression] = Seq(a)

  /**
    * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
    * This function should return a value that is of the class as the object that was given to it.
    */
  override def mapSubexpressions(func: (Expression) => Expression): FloatExpression = func(a).asInstanceOf[FloatExpression]

  /**
    * Returns true if continuous (not an integer variable)
    */
  override def continuous: Boolean = subexpressions().forall(_.continuous)

  /**
    * Returns true if the expression is linear
    */
  override def linear: Boolean = false

  /**
    * True if the variable is bound
    */
  override def isBound: Boolean = subexpressions().forall(_.isBound)
}