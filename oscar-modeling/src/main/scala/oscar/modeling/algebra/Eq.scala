package oscar.modeling.algebra

import oscar.modeling.misc.VariableNotBoundException

/**
 * a == b
 */
case class Eq(v: Array[IntExpression]) extends BoolExpression {
  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  override def evaluateBool(): Boolean = {
    val eval = v.map(_.evaluate())
    eval.forall(x => x == eval(0))
  }

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = v

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = Eq(v.map(func))
}

object Eq {
  def apply(a: IntExpression*): Eq = Eq(a.toArray)

  def apply(v: Iterable[IntExpression]): Eq = Eq(v.toArray)

  def apply[A](indices: Iterable[A])(f: A => IntExpression): Eq = Eq(indices map f)

  def apply[A, B](indices1: Iterable[A], indices2: Iterable[B])(f: (A, B) => IntExpression): Eq = Eq(for (i <- indices1; j <- indices2) yield f(i, j))

  def apply(n1: Int, n2: Int)(f: (Int, Int) => IntExpression): Eq = Eq(0 until n1, 0 until n2)(f)
}