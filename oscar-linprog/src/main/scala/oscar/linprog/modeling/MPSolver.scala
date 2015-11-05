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

import java.nio.file.Path

import oscar.algebra.{Const, LinearExpression}
import oscar.linprog.enums._
import oscar.linprog.interface._

import scala.util.{Failure, Success, Try}

/**
 * A solver that can be used to solve mathematical programming problems.
 *
 * @author acrucifix acr@n-side.com
 */
class MPSolver[I <: MPSolverInterface](val solverInterface: I) {

  protected def dirty: Boolean = solveStatus == NotSolved
  protected def setDirty() = {
    _solveStatus = NotSolved
    infeasibilitiesFound = false
  }

  /**
   * Returns the name of the current model
   */
  def modelName = solverInterface.modelName

  /**
   * Sets the name of the current model to the given value
   */
  def modelName_=(value: String) = solverInterface.modelName = value


  /* OBJECTIVE */

  private var _objective: LinearExpression = Const(1)

  /**
   * Returns the [[LinearExpression]] representing the current objective of the problem
   */
  def objective: LinearExpression = _objective

  /**
   * Sets the optimization objective and direction to the given values.
   *
   * @param obj the new objective expression
   * @param min the new optimization direction (true for minimization and false for maximization)
   */
  def setObjective(obj: LinearExpression, min: Boolean) = {
    setDirty()

    _objective = obj

    val (varIds, coefs) = obj.coef.map { case(vari, coef) => (variableColumn(vari.name), coef)}.unzip
    solverInterface.setObjective(min, coefs.toArray, varIds.toArray)
  }

  /**
   * Returns the value of the objective in the solution found to the current model (if any)
   */
  def objectiveValue = asSuccessIfSolFound(solverInterface.objectiveValue)


  /* VARIABLES */

  protected var variables = Map[String, MPVar[I]]()
  protected var variableColumn = Map[String, Int]()

  def getNumberOfVariables = {
    assert(variables.size == solverInterface.getNumberOfVariables,
      "The number of variables stored does not correspond to the number of variables added to the solver.")

    variables.size
  }

  protected def register(variable: MPVar[I], colId: Int): Unit = {
    setDirty()

    require(!variables.contains(variable.name), s"There exists already a variable with name ${variable.name}.")

    variables += (variable.name -> variable)
    variableColumn += (variable.name -> colId)
  }

  /**
   * Adds the given [[MPFloatVar]] to the problem
   */
  def addFloatVar(variable: MPVar[I]) = {
    require(variable.initialVarType == Continuous, "Cannot add a non continuous variable using addFloatVar")

    val colId = solverInterface.addVariable(variable.name, variable.initialLowerBound, variable.initialUpperBound)

    register(variable, colId)
  }

  /**
   * Adds the given [[MPIntVar]] to the problem
   */
  def addIntVar(variable: MPVar[I])(implicit ev: I => MIPSolverInterface) = {
    require(variable.initialVarType == Integer, "Cannot add a non integer variable using addIntVar")
    require(variable.initialLowerBound.isValidInt, "The lower bound of an integer variable should be an integral number.")
    require(variable.initialUpperBound.isValidInt, "The upper bound of an integer variable should be an integral number.")

    val colId = ev(solverInterface).addIntegerVariable(variable.name, variable.initialLowerBound.toInt, variable.initialUpperBound.toInt)

    register(variable, colId)
  }

  /**
   * Adds the given [[MPBinaryVar]] to the problem.
   */
  def addBinaryVar(variable: MPVar[I])(implicit ev: I => MIPSolverInterface) = {
    require(variable.initialVarType == Binary, "Cannot add a non binary variable using addBinaryVar")

    val colId = solverInterface.addBinaryVariable(variable.name)

    register(variable, colId)
  }

  /**
   * Returns the [[MPVar]] corresponding to the given name.
   */
  def variable(name: String) = variables(name)

  /**
   * Returns the type of the variable with the given name.
   */
  def getVariableType(varName: String)(implicit ev: I => MIPSolverInterface): MPVarType = {
    if(isInteger(varName))     Integer
    else if(isBinary(varName)) Binary
    else                       Continuous
  }

  /**
   * Return true if the variable with the given name is of type [[Continuous]]
   */
  def isFloat(varName: String)(implicit ev: I => MIPSolverInterface): Boolean = ev(solverInterface).isFloat(variableColumn(varName))

  /**
   * Return true if the variable with the given name is of type [[Integer]]
   */
  def isInteger(varName: String)(implicit ev: I => MIPSolverInterface): Boolean = ev(solverInterface).isInteger(variableColumn(varName))

  /**
   * Return true if the variable with the given name is of type [[Binary]]
   */
  def isBinary(varName: String)(implicit ev: I => MIPSolverInterface): Boolean = ev(solverInterface).isBinary(variableColumn(varName))

  /**
   * Updates the type of the variable
   */
  def setVariableType(varName: String, varType: MPVarType)(implicit ev: I => MIPSolverInterface) = {
    setDirty()

    if (getVariableType(varName) != varType) {
      varType match {
        case Continuous => ev(solverInterface).setFloat(variableColumn(varName))
        case Integer    => ev(solverInterface).setInteger(variableColumn(varName))
        case Binary     => ev(solverInterface).setBinary(variableColumn(varName))
      }
    }
  }

  /**
   * Updates the type of the given variable to [[Continuous]]
   */
  def setFloat(varName: String)(implicit ev: I => MIPSolverInterface) = setVariableType(varName, Continuous)

  /**
   * Updates the type of the given variable to [[Integer]]
   */
  def setInteger(varName: String)(implicit ev: I => MIPSolverInterface) = setVariableType(varName, Integer)

  /**
   * Updates the type of the given variable to [[Binary]]
   */
  def setBinary(varName: String)(implicit ev: I => MIPSolverInterface) = setVariableType(varName, Binary)

  /**
   * Returns the lower bound of the variable with the given name.
   */
  def getVariableLowerBound(varName: String): Double = solverInterface.getVariableLowerBound(variableColumn(varName))

  /**
   * Updates the lower bound of the variable with the given name to the given value
   *
   * @param varName the name of the variable that should be updated
   * @param lb the new value of the lower bound for this variable
   */
  def setVariableLowerBound(varName: String, lb: Double) = {
    setDirty()

    solverInterface.setVariableLowerBound(variableColumn(varName), lb)
  }

  /**
   * Returns the upper bound of the variable with the given name.
   */
  def getVariableUpperBound(varName: String): Double = solverInterface.getVariableUpperBound(variableColumn(varName))

  /**
   * Updates the upper bound of the variable with the given name to the given value
   *
   * @param varName the name of the variable that should be updated
   * @param ub the new value of the upper bound for this variable
   */
  def setVariableUpperBound(varName: String, ub: Double) = {
    setDirty()

    solverInterface.setVariableUpperBound(variableColumn(varName), ub)
  }

  /**
   * Returns the value of the variable with the given name in the current solution (if any)
   */
  def value(varName: String): Try[Double] = endStatus.flatMap { status =>
    if (status == SolutionFound) Success(solverInterface.getVariableValue(variableColumn(varName)))
    else                         Failure(NoSolutionFoundException(status))
  }


  /* CONSTRAINTS */

  protected var linearConstraints = Map[String, LinearConstraint[I]]()
  protected var linearConstraintRows = Map[String, Int]()

  def getNumberOfLinearConstraints = {
    assert(linearConstraints.size == solverInterface.getNumberOfLinearConstraints,
      "The number of linear constraints stored does not correspond to the number of linear constraints added to the solver.")

    linearConstraints.size
  }

  protected def register(linearConstraint: LinearConstraint[I], rowId: Int): Unit = {
    setDirty()

    require(!linearConstraints.contains(linearConstraint.name), s"There exists already a linear constraint with name ${linearConstraint.name}.")

    linearConstraints += (linearConstraint.name -> linearConstraint)
    linearConstraintRows += (linearConstraint.name -> rowId)
  }

  /**
   * Adds the given [[LinearConstraint]] to the model
   */
  def add(linearConstraint: LinearConstraint[I]) = {
    val (varIds, coefs) = linearConstraint.constraintExpr.linExpr.coef.map {
      case (vari, coef) => (variableColumn(vari.name), coef)
    }.unzip

    val rowId = solverInterface.addConstraint(linearConstraint.name, coefs.toArray, varIds.toArray, linearConstraint.constraintExpr.sense.symbol, -linearConstraint.constraintExpr.linExpr.cte)

    register(linearConstraint, rowId)
  }


  /* SOLVE */

  private var _solveStatus: SolveStatus = NotSolved

  /**
   * Returns the current status of the solve. (see [[SolveStatus]])
   */
  def solveStatus = _solveStatus

  /**
   * Solves the current optimization problem
   *
   * @return the [[EndStatus]] of the solve
   */
  def solve: EndStatus = {
    solverInterface.updateModel()
    val es = solverInterface.solve
    _solveStatus = Solved
    _endStatus = Success(es)
    es
  }

  /**
   * Aborts the current solve (if any).
   *
   * The method gracefully terminates the previous unterminated call to [[MPSolver.solve]].
   * The method has no effect on successive calls to [[MPSolver.solve]].
   */
  def abort(): Unit = solverInterface.abort()

  /**
   * Returns true if the current problem has been solved
   */
  def solved: Boolean = solveStatus == Solved

  private var _endStatus: Try[EndStatus] = Failure(NotSolvedYetException)

  /**
   * Returns the end status of the last solve (if any)
   */
  def endStatus = _endStatus

  /**
   * Returns true if there is a solution to the current problem
   */
  def hasSolution: Boolean = solved && endStatus == Success(SolutionFound)

  protected def asSuccessIfSolFound[B](value: B): Try[B] = endStatus.flatMap { status =>
    if (status == SolutionFound) Success(value)
    else                    Failure(NoSolutionFoundException(status))
  }

  /**
   * Returns the [[SolutionQuality]] of the solution if any
   */
  def solutionQuality: Try[SolutionQuality] = asSuccessIfSolFound(solverInterface.solutionQuality)

  /**
   * Releases the raw solver interfaced by OscaR. This may be needed by some solvers that use native resources.
   */
  def release() = solverInterface.release()

  /**
   * Saves the problem to the file at the given path in the given format.
   */
  def exportModel(filepath: Path, format: ModelExportFormat): Unit = {
    if(dirty) solverInterface.updateModel()

    solverInterface.exportModel(filepath, format)
  }


  /* INFEASIBILITY ANALYSIS */

  private var infeasibilitiesFound = false

  protected def asSuccessIfInfeasFound[B](value: B): Try[B] = {
    if (infeasibilitiesFound) Success(value)
    else                      Failure(NoInfeasibilityFoundException)
  }

  /**
   * Finds the sources of infeasibilities in the problem.
   */
  def analyseInfeasibility()(implicit ev: I => InfeasibilityAnalysisInterface): Try[InfeasibleSet] =
    if(endStatus == Success(Infeasible)) {
      val success = ev(solverInterface).analyseInfeasibility()
      if(success) {
        infeasibilitiesFound = true

        val linearCstrs = linearConstraints.values.filter(_.infeasible.get).map(_.name).toSeq
        val lowerBounds = variables.values.filter(_.lowerBoundInfeasible.get).map(_.name).toSeq
        val upperBounds = variables.values.filter(_.upperBoundInfeasible.get).map(_.name).toSeq

        Success(InfeasibleSet(linearCstrs, lowerBounds, upperBounds))
      } else {
        Failure(NoInfeasibilityFoundException)
      }
    } else  {
      Failure(new IllegalArgumentException("Warning: the problem should be infeasible in order to analyze infeasibilities."))
    }

  /**
   * Returns true if the lower bound of the given variable
   * belongs to the set of constraints making the problem infeasible
   */
  def isVariableLowerBoundInfeasible(varName: String)(implicit ev: I => InfeasibilityAnalysisInterface): Try[Boolean] =
    asSuccessIfInfeasFound(ev(solverInterface).isVariableLowerBoundInfeasible(variableColumn(varName)))

  /**
   * Returns true if the upper bound of the given variable
   * belongs to the set of constraints making the problem infeasible
   */
  def isVariableUpperBoundInfeasible(varName: String)(implicit ev: I => InfeasibilityAnalysisInterface): Try[Boolean] =
    asSuccessIfInfeasFound(ev(solverInterface).isVariableUpperBoundInfeasible(variableColumn(varName)))

  /**
   * Returns true if the given constraint
   * belongs to the set of constraints making the problem infeasible
   */
  def isLinearConstraintInfeasible(cstrName: String)(implicit ev: I => InfeasibilityAnalysisInterface): Try[Boolean] =
    asSuccessIfInfeasFound(ev(solverInterface).isLinearConstraintInfeasible(linearConstraintRows(cstrName)))
}
