package oscar.modeling.models.cp

import oscar.algo.reversible.ReversibleArrayStack
import oscar.modeling.constraints.Constraint
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.modeling.solvers.cp.Branchings.Alternative

object MemoCPModel {
  def apply(implicit modelDeclaration: ModelDeclaration): MemoCPModel = {
    new MemoCPModel(modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel])
  }
}

/**
  * CPModel that memorizes added constraint, with a support for pushing/popping state.
  * @param base model to instantiate
  */
class MemoCPModel(base: UninstantiatedModel) extends CPModel(base) {
  //private var currentConstraintList = List[Constraint]()
  //private val constraintListHistory = mutable.Stack[List[Constraint]]()
  private val constraintList = new ReversibleArrayStack[Constraint](cpSolver)

  /**
    * Post a new constraint
    *
    * @param constraint constraint to add
    */
  override def post(constraint: Constraint): Unit = {
    super.post(constraint)
    if(constraintList != null) //only add once the model is init
      constraintList.push(constraint)
  }

  /**
    * Push the current state (to be able to recover it later using popState()).
    * Save the current CP domains and added constraint list.
    */
  def pushState(): Unit = {
    cpSolver.pushState()
  }

  /**
    * Recover the previous state from the stack. Restore current CP domain and added constraint list
    */
  def popState(): Unit = {
    cpSolver.pop()
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
  def getAddedConstraints: List[Constraint] = constraintList.toArray.toList
}
