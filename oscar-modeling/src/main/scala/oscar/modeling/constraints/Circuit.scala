package oscar.modeling.constraints

import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.algebra.integer.IntExpression

case class Circuit(succ: Array[IntExpression], symmetric: Boolean = true) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = succ

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}
case class SubCircuit(succ: Array[IntExpression], offset: Int = 0) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = succ

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}