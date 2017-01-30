package oscar.modeling.constraints

import oscar.cp.CPSolver
import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.algebra.integer.IntExpression

/**
  * A custom constraint that can be instantiated in a CP model
  */
trait CPInstantiableConstraint extends Constraint {
  /**
    * Posts the constraint in the CP solver and returns its outcome
    * @return
    */
  def cpPost(cpSolver: CPSolver): Unit

  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = ???

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = ???
}
