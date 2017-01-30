package oscar.modeling.constraints

import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.algebra.integer.IntExpression

/**
 * Common trait of all constraints
 */
trait Constraint extends Serializable {
  /**
   * @return a list of all the IntExpression associated to this constraint
   * The implementation of this method is not mandatory.
   */
  def getIntExpressions(): Iterable[IntExpression] = throw new RuntimeException("Not implemented")

  /**
   * @return a list of all the FloatExpression associated to this constraint
   * The implementation of this method is not mandatory.
   */
  def getFloatExpressions(): Iterable[FloatExpression] = throw new RuntimeException("Not implemented")
}