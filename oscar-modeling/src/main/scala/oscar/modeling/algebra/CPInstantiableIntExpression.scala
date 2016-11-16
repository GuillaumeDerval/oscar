package oscar.modeling.algebra

import oscar.algo.search.Outcome
import oscar.cp._
import oscar.cp.core.variables.CPIntVar

/**
  * An IntExpression that can be instantiated in a CP model
  */
trait CPInstantiableIntExpression extends IntExpression {
  /**
    * Post the expression, and return a CPIntVar corresponding to its value
    */
  def cpPostAndGetVar(cPSolver: CPSolver): CPIntVar

  /**
    * Post the expression, with 'v' being the value the expression should equal to
    * @param v The value the expression should equal to
    */
  def cpPostWithVar(cPSolver: CPSolver, v: CPIntVar): Outcome
}
