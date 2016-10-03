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

package oscar.linprog.modeling

import oscar.linprog.interface.{InfeasibilityAnalysisInterface, MIPSolverInterface, MPSolverInterface}

import scala.language.implicitConversions

/**
 * Describes the type of an MPVar: [[Continuous]], [[Integer]] or [[Binary]]
 *
 * @author acrucifix acr@n-side.com
 */
sealed abstract class MPVarType(name: String) {
  def createVar[I <: MPSolverInterface](variable: MPVar[I])(implicit solver: MPSolver[I])

  override def toString = name
}

/**
 * Variables of this type can take values in the real domain.
 *
 * @author acrucifix acr@n-side.com
 */
case object Continuous extends MPVarType("continuous") {
  def createVar[I <: MPSolverInterface](variable: MPVar[I])(implicit solver: MPSolver[I]) =
    solver.addFloatVar(variable)
}

/**
 * Variables of this type can take values in the integer domain.
 *
 * @author acrucifix acr@n-side.com
 */
case object Integer extends MPVarType("integer") {
  // If we are in the case of an Integer variable, we know that the solver interface is a MIPSolverInterface
  implicit def convertSolver[I <: MPSolverInterface](s: I): MIPSolverInterface = s.asInstanceOf[MIPSolverInterface]

  def createVar[I <: MPSolverInterface](variable: MPVar[I])(implicit solver: MPSolver[I]) =
    solver.addIntVar(variable)
}

/**
 * Variables of this type can take values in the binary (0-1) domain.
 *
 * @author acrucifix acr@n-side.com
 */
case object Binary extends MPVarType("binary") {
  // If we are in the case of a Binary variable, we know that the solver interface is a MIPSolverInterface
  implicit def convertSolver[I <: MPSolverInterface](s: I): MIPSolverInterface = s.asInstanceOf[MIPSolverInterface]

  def createVar[I <: MPSolverInterface](variable: MPVar[I])(implicit solver: MPSolver[I]) =
    solver.addBinaryVar(variable)
}

/**
 * Represents a variable in a mathematical programming model.
 *
 * @author acrucifix acr@n-side.com
 */
class MPVar[+I <: MPSolverInterface] private (val initialVarType: MPVarType, val name: String, val initialLowerBound: Double, val initialUpperBound: Double)(implicit solver: MPSolver[I]) extends oscar.algebra.Var[Double] {
  initialVarType.createVar(this)

  def id = 0
  /**
   * Returns the [[MPVarType]] of this variable.
   */
  def variableType(implicit ev: I => MIPSolverInterface): MPVarType = solver.getVariableType(name)

  /**
   * Returns true if this variable is of type [[Continuous]]
   */
  def float(implicit ev: I => MIPSolverInterface): Boolean = solver.isFloat(name)

  /**
   * Returns true if this variable is of type [[Integer]]
   */
  def integer(implicit ev: I => MIPSolverInterface): Boolean = solver.isInteger(name)

  /**
   * Returns true if this variable is of type [[Binary]]
   */
  def binary(implicit ev: I => MIPSolverInterface): Boolean = solver.isBinary(name)

  /**
   * Sets the [[MPVarType]] of this variable to the given type.
   */
  def variableType_=(value: MPVarType)(implicit ev: I => MIPSolverInterface) = solver.setVariableType(name, value)

  /**
   * Sets the [[MPVarType]] of this variable to [[Continuous]]
   */
  def setFloat()(implicit ev: I => MIPSolverInterface) = solver.setFloat(name)

  /**
   * Sets the [[MPVarType]] of this variable to [[Integer]]
   */
  def setInteger()(implicit ev: I => MIPSolverInterface) = solver.setInteger(name)

  /**
   * Sets the [[MPVarType]] of this variable to [[Binary]]
   */
  def setBinary()(implicit ev: I => MIPSolverInterface) = solver.setBinary(name)

  /**
   * Returns the lower bound.
   */
  def lowerBound = solver.getVariableLowerBound(name)

  /**
   * Sets the lower bound to the given value.
   */
  def lowerBound_=(value: Double) = solver.setVariableLowerBound(name, value)

  /**
   * Returns the upper bound.
   */
  def upperBound = solver.getVariableUpperBound(name)

  /**
   * Sets the upper bound to the given value.
   */
  def upperBound_=(value: Double) = solver.setVariableUpperBound(name, value)

  /**
   * Returns the bounds of this variable (lower, upper)
   */
  def bounds = (lowerBound, upperBound)

  /**
   * Sets the bounds of this variable to the given value.
   *
   * @param lb = the new value for the lower bound
   * @param ub = the new value for the upper bound
   */
  def setBounds(lb: Double, ub: Double) = {
    lowerBound = lb
    upperBound = ub
  }

  /**
   * Resets the bounds of this variable to their initial value
   */
  def resetBounds() = {
    lowerBound = initialLowerBound
    upperBound = initialUpperBound
  }

  /**
   * Returns the value of this variable in the solution found by the solver if any
   */
  def value: Option[Double] = solver.value(name).toOption

  /**
   * Returns true in case the lower bound on this variable belongs to the set of infeasible constraints
   */
  def lowerBoundInfeasible(implicit ev: I => InfeasibilityAnalysisInterface): Option[Boolean] = solver.isVariableLowerBoundInfeasible(name).toOption

  /**
   * Returns true in case the upper bound on this variable belongs to the set of infeasible constraints
   */
  def upperBoundInfeasible(implicit ev: I => InfeasibilityAnalysisInterface): Option[Boolean] = solver.isVariableUpperBoundInfeasible(name).toOption
}

/**
 * Creates variables
 *
 * @author acrucifix acr@n-side.com
 */
object MPVar {
  def float[I <: MPSolverInterface](name: String, lb: Double = -Double.MaxValue, ub: Double = Double.MaxValue)(implicit solver: MPSolver[I]): MPVar[I] =
    new MPVar[I](Continuous, name, lb, ub)

  def int[I <: MIPSolverInterface](name: String, from: Int, to: Int)(implicit solver: MPSolver[I]): MPVar[I] =
    new MPVar[I](Integer, name, from, to)

  def binary[I <: MIPSolverInterface](name: String)(implicit solver: MPSolver[I]): MPVar[I] =
    new MPVar[I](Binary, name, 0.0, 1.0)
}

/**
 * Creates continuous variables
 *
 * @author acrucifix acr@n-side.com
 */
object MPFloatVar {
  def apply[I <: MPSolverInterface](name: String, lb: Double = -Double.MaxValue, ub: Double = Double.MaxValue)(implicit solver: MPSolver[I]): MPVar[I] =
    MPVar.float(name, lb, ub)
}

/**
 * Creates integer variables
 *
 * @author acrucifix acr@n-side.com
 */
object MPIntVar {
  def apply[I <: MIPSolverInterface](name: String, from: Int, to: Int)(implicit solver: MPSolver[I]): MPVar[I] =
    MPVar.int(name, from, to)
  def apply[I <: MIPSolverInterface](name: String, range: Range)(implicit solver: MPSolver[I]): MPVar[I] =
    apply(name, range.min, range.max)
}

/**
 * Creates binary (0-1) integer variables
 *
 * @author acrucifix acr@n-side.com
 */
object MPBinaryVar {
  def apply[I <: MIPSolverInterface](name: String)(implicit solver: MPSolver[I]): MPVar[I] =
    MPVar.binary(name)
}
