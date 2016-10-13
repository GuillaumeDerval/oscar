package oscar.modeling.models

import oscar.modeling.constraints.Constraint
import oscar.modeling.solvers.cp.branchings.Branching.Alternative

import scala.collection.mutable

/**
  * CPModel that memorizes added constraint, with a support for pushing/popping state.
  * @param base model to instantiate
  */
class MemoCPModel(base: UninstantiatedModel) extends CPModel(base) {
  private var currentConstraintList = List[Constraint]()
  private val constraintListHistory = mutable.Stack[List[Constraint]]()

  /**
    * Post a new constraint
    *
    * @param constraint constraint to add
    */
  override def post(constraint: Constraint): Boolean = {
    val out = super.post(constraint)
    if(out && currentConstraintList != null) //only add once the model is init
      currentConstraintList = constraint :: currentConstraintList
    out
  }

  /**
    * Push the current state (to be able to recover it later using popState()).
    * Save the current CP domains and added constraint list.
    */
  def pushState(): Unit = {
    cpSolver.pushState()
    constraintListHistory.push(currentConstraintList)
  }

  /**
    * Recover the previous state from the stack. Restore current CP domain and added constraint list
    */
  def popState(): Unit = {
    cpSolver.pop()
    currentConstraintList = constraintListHistory.pop()
  }

  /**
    * Apply a branching
    * @param branching the sequence of alternatives to apply
    */
  def apply(branching: Seq[Alternative]): Unit = {
    for(a <- branching) {
      pushState()
      a()
      popState()
    }
  }

  /**
    * @return the constraints added since the instantiation of the model
    */
  def getAddedConstraints: List[Constraint] = currentConstraintList
}
