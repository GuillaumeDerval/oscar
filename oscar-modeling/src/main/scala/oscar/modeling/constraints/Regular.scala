package oscar.modeling.constraints

import oscar.cp.constraints.Automaton
import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.algebra.integer.IntExpression

/**
 * Impose that the values of the variables in 'on' can be emitted by a particular automaton
 */
case class Regular(on: Array[IntExpression], automaton: Automaton) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = on

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}