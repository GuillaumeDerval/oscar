package oscar.modeling.constraints

import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.algebra.integer.IntExpression

/**
  * Created by dervalguillaume on 8/06/16.
  */
case class Spread(x: Iterable[IntExpression], s: Int, s2: IntExpression) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = x

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}
