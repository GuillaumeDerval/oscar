package oscar.modeling.algebra

import oscar.algo.search.Outcome
import oscar.cp.{CPBoolVar, CPSolver}

/**
  * A BoolExpression that can be instantiated in a CP model
  */
trait CPInstantiableBoolExpression extends BoolExpression {
  /**
    * Post the expression as a constraint (meaning the expression should be true)
    */
  def cpPostAsConstraint(cPSolver: CPSolver): Outcome

  /**
    * Post the expression, and return a CPIntVar corresponding to its value
    */
  def cpPostAndGetVar(cPSolver: CPSolver): CPBoolVar

  /**
    * Post the expression, with 'v' being the value the expression should equal to
    * @param v The value the expression should equal to
    */
  def cpPostWithVar(cPSolver: CPSolver, v: CPBoolVar): Outcome
}
