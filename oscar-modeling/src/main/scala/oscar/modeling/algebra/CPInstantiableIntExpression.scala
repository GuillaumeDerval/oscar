package oscar.modeling.algebra

import oscar.cp.core.variables.CPIntVar

/**
  * An IntExpression that can be instantiated in a CP model
  */
trait CPInstantiableIntExpression extends IntExpression {
  /**
    * Post the expression, and return a CPIntVar corresponding to its value
    */
  def postAndGetVar(): CPIntVar

  /**
    * Post the expression, with 'v' being the value the expression should equal to
    * @param v The value the expression should equal to
    */
  def postWithVar(v: CPIntVar): Unit
}
