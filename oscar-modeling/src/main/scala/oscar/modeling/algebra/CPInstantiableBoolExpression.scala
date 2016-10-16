package oscar.modeling.algebra

import oscar.cp.CPBoolVar
import oscar.cp.core.CPOutcome

/**
  * A BoolExpression that can be instantiated in a CP model
  */
trait CPInstantiableBoolExpression extends BoolExpression {
  /**
    * Post the expression as a constraint (meaning the expression should be true)
    */
  def postAsConstraint(): CPOutcome

  /**
    * Post the expression, and return a CPIntVar corresponding to its value
    */
  def postAndGetVar(): CPBoolVar

  /**
    * Post the expression, with 'v' being the value the expression should equal to
    * @param v The value the expression should equal to
    */
  def postWithVar(v: CPBoolVar): Unit
}
