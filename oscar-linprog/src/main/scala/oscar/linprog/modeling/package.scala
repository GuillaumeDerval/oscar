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
import oscar.linprog.interface.MPSolverInterface

/**
 * Helper functions to build models within the context of an implicit solver
 */
package object modeling {

  def minimize[I <: MPSolverInterface](expr: LinearExpression)(implicit solver: MPSolver[I]) = solver.setObjective(expr, min = true)
  def maximize[I <: MPSolverInterface](expr: LinearExpression)(implicit solver: MPSolver[I]) = solver.setObjective(expr, min = false)

  def add[I <: MPSolverInterface](cstr: LinearConstraintExpression, name: String = "")(implicit solver: MPSolver[I]): LinearConstraint[I] = {
    val n =
      if(name == "") "cstr" + solver.getNumberOfLinearConstraints
      else name

    LinearConstraint(n, cstr)
  }

  def subjectTo[I <: MPSolverInterface](cstrs: (String, LinearConstraintExpression)*)(implicit solver: MPSolver[I]): Seq[(String, LinearConstraint[I])] =
    cstrs.toSeq map { case (name, cstr) =>
      name -> add(cstr, name)
    }

  def subjectTo[I <: MPSolverInterface](cstrs: LinearConstraintExpression*)(implicit solver: MPSolver[I]): IndexedSeq[LinearConstraint[I]] =
    cstrs.toIndexedSeq map { cstr =>
      add(cstr)
    }
}
