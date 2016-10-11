package algebra

import misc.VariableNotBoundException

case class Abs(a: IntExpression) extends IntExpression {
  /**
    * Evaluate this expression. All variables referenced have to be bound.
    *
    * @throws VariableNotBoundException when a variable is not bound
    * @return the value of this expression
    */
  override def evaluate(): Int = math.abs(a.evaluate())

  /**
    * Return a lower bound for this expression
    */
  override def min: Int = values().min

  /**
    * Return a higher bound for this expression
    */
  override def max: Int = {
    if(math.abs(a.max) > math.abs(a.min)) math.abs(a.max)
    else math.abs(a.min)
  }

  /**
    * Returns an iterable that contains a superset of the values this expression can have
    */
  override def values(): Iterable[Int] = a.values().map(x => math.abs(x))

  /**
    * Returns an iterable that contains all sub-expressions of this expression
    */
  override def subexpressions(): Iterable[IntExpression] = Seq(a)

  /**
    * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
    * This function should return a value that is of the class as the object that was given to it.
    */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = func(a)
}