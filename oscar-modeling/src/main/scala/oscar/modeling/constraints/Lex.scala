package oscar.modeling.constraints

import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.algebra.integer.IntExpression

/**
  * Lexicographical Less or Equal constraint.
  *
  * Given tuples (a1, a2, ...) and (b1, b2, ...),
  * this ensures
  *
  * a1 <= b1 || (a1 == b1 && LexLeq((a2, ...), (b2, ...)))
 *
  * @param a
  * @param b
  */
case class LexLeq(a: Array[IntExpression], b: Array[IntExpression]) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = a ++ b

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}

object LexGeq
{
  /**
    * Lexicographical "Greater or Equal than" constraint.
    *
    * Given tuples (a1, a2, ...) and (b1, b2, ...),
    * this ensures
    *
    * a1 <= b1 || (a1 == b1 && LexGeq((a2, ...), (b2, ...)))
    * @param a
    * @param b
    */
  def apply(a: Array[IntExpression], b: Array[IntExpression]) = LexLeq(b,a)
}

object LexLr
{
  /**
    * Lexicographical "Strictly Lesser than" constraint.
    *
    * Given tuples (a1, a2, ...) and (b1, b2, ...),
    * this ensures
    *
    * a1 < b1 || (a1 == b1 && LexLr((a2, ...), (b2, ...)))
    * @param a
    * @param b
    */
  def apply(a: Array[IntExpression], b: Array[IntExpression]) = LexLeq(a.map(_+1),b)
}

object LexGr
{
  /**
    * Lexicographical "Strictly Greater than" constraint.
    *
    * Given tuples (a1, a2, ...) and (b1, b2, ...),
    * this ensures
    *
    * a1 > b1 || (a1 == b1 && LexGr((a2, ...), (b2, ...)))
    * @param a
    * @param b
    */
  def apply(a: Array[IntExpression], b: Array[IntExpression]) = LexLr(b.map(_+1),a)
}