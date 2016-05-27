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

package oscar.linprog

import oscar.algebra._
import oscar.linprog.interface.{MIPSolverInterface, MPSolverInterface}

/**
 * Helper functions to build models within the context of an implicit solver
 */
package object modeling {

  /**
   * Replaces the objective of the solver by the minimization of the given [[LinearExpression]]
   */
  def minimize[I <: MPSolverInterface](expr: LinearExpression)(implicit solver: MPSolver[I]) = solver.setObjective(expr, min = true)

  /**
   * Replaces the objective of the solver by the maximization of the given [[LinearExpression]]
   */
  def maximize[I <: MPSolverInterface](expr: LinearExpression)(implicit solver: MPSolver[I]) = solver.setObjective(expr, min = false)

  /**
   * Adds the constraints described by the given [[LinearConstraintExpression]] to the model and returns the corresponding [[LinearConstraint]].
   *
   * In case no name is given to the constraint, params name equals to the empty string,
   * a default name is created by appending to "cstr" the row number of the linear constraint in the matrix of the problem.
   */
  def add[I <: MPSolverInterface](cstr: LinearConstraintExpression, name: String = "")(implicit solver: MPSolver[I]): LinearConstraint[I] = {
    val n =
      if(name == "") "cstr" + solver.getNumberOfLinearConstraints
      else name

    LinearConstraint(n, cstr)
  }

  /**
   * Adds the given [[LinearConstraintExpression]] to the model with given names.
   */
  def subjectTo[I <: MPSolverInterface](cstrs: (String, LinearConstraintExpression)*)(implicit solver: MPSolver[I]): Seq[(String, LinearConstraint[I])] =
    cstrs.toSeq map { case (name, cstr) =>
      name -> add(cstr, name)
    }

  /**
   * Adds the given [[LinearConstraintExpression]] to the model with default names.
   */
  def subjectTo[I <: MPSolverInterface](cstrs: LinearConstraintExpression*)(implicit solver: MPSolver[I]): IndexedSeq[LinearConstraint[I]] =
    cstrs.toIndexedSeq map { cstr =>
      add(cstr)
    }

  /**
   * Adds a new absolute value expression to the model and returns the [[LinearExpression]] associated.
   */
  def abs[I <: MIPSolverInterface](linearExpression: LinearExpression, lb: Double, ub: Double)(implicit solver: MPSolver[I]): LinearExpression = {
    val absName = solver.addAbsExpression(linearExpression, lb, ub)

    solver.piecewiseExpr(absName)
  }

  /**
   * Adds a new sign expression to the model and returns the [[LinearExpression]] associated.
   */
  def sign[I <: MIPSolverInterface](linearExpression: LinearExpression, lb: Double, ub: Double)(implicit solver: MPSolver[I]): LinearExpression = {
    val absName = solver.addSignExpression(linearExpression, lb, ub)

    solver.piecewiseExpr(absName)
  }
}
