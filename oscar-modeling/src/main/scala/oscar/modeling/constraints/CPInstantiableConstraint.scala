package oscar.modeling.constraints

import oscar.cp.CPSolver
import oscar.cp.core.CPOutcome

/**
  * A custom constraint that can be instantiated in a CP model
  */
trait CPInstantiableConstraint extends Constraint {
  /**
    * Posts the constraint in the CP solver and returns its outcome
    * @return
    */
  def cpPost(cpSolver: CPSolver): CPOutcome
}
