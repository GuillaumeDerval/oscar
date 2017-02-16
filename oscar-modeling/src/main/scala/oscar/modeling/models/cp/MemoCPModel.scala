/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

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
