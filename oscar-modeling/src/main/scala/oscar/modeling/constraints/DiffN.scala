package oscar.modeling.constraints

import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.algebra.integer.IntExpression

/**
  * Non overlapping between 2D rectangles
  *
  * @param x is the x coordinates of the bottom left corner of rectangles
  * @param dx is the length in direction of x of each rectangle
  * @param y is the y coordinates of the bottom left corner of rectangles
  * @param dy is the length in direction y of each rectangle
  * @return a set of constraints such that posting all of them enforces the non overlapping of rectangles
  */
case class DiffN(x: Array[IntExpression], dx: Array[IntExpression], y: Array[IntExpression], dy: Array[IntExpression]) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = x ++ dx ++ y ++ dy

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}
