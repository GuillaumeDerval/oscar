package oscar.modeling.constraints

import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.algebra.integer.IntExpression

/**
 * Imposes that a given BoolExpression is true
 *
 * @param array
 */
case class StrongEq(a: IntExpression, b: IntExpression) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = Array(a, b)

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}
