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

  private def dirty: Boolean = solveStatus == NotSolved
  private def setDirty() = {
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

  private var variables = Map[String, MPVar[I]]()
  private var variableColumn = Map[String, Int]()

  def getNumberOfVariables = {
    assert(variables.size == solverInterface.getNumberOfVariables,
      "The number of variables stored does not correspond to the number of variables added to the solver.")

    variables.size
  }

  private def register(variable: MPVar[I], colId: Int): Unit = {
    setDirty()

    require(!variables.contains(variable.name), s"There exists already a variable with name ${variable.name}.")

    variables += (variable.name -> variable)
    variableColumn += (variable.name -> colId)
  }

  /**
   * Removes the variable with the given name from the model (if any).
   *
   * The variable should not be used in the objective or in any constraint.
   */
  def removeVariable(varName: String): Unit =
    if(variables.contains(varName)) {
      setDirty()

      val varId = variableColumn(varName)

      val v = variable(varName)
      require(
        !objective.uses(v) &&
        linearConstraints.values.forall(c => !c.expression.linExpr.uses(v)),
        s"Cannot remove variable $varName because it is either used in the objective or in a constraint. Please remove the objective or the constraint first."
      )

      solverInterface.removeVariable(varId)

      variables -= varName
      variableColumn -= varName
      variableColumn = variableColumn.mapValues(colId => if(colId > varId) colId - 1 else colId)
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

  /* LINEAR CONSTRAINTS */
  private var linearConstraints = Map[String, LinearConstraint[I]]()
  private var linearConstraintRow = Map[String, Int]()

  def getNumberOfLinearConstraints = {
    assert(linearConstraints.size == solverInterface.getNumberOfLinearConstraints,
      "The number of linear constraints stored does not correspond to the number of linear constraints added to the solver.")

    linearConstraints.size
  }

  private def register(linearConstraint: LinearConstraint[I], rowId: Int): Unit = {
    setDirty()

    require(!linearConstraints.contains(linearConstraint.name), s"There exists already a linear constraint with name ${linearConstraint.name}.")

    linearConstraints += (linearConstraint.name -> linearConstraint)
    linearConstraintRow += (linearConstraint.name -> rowId)
  }

  /**
   * Removes the constraint with the given name from the model (if any).
   */
  def removeLinearConstraint(cstrName: String) =
    if(linearConstraints.contains(cstrName)) {
      setDirty()

      val cstrId = linearConstraintRow(cstrName)

      solverInterface.removeConstraint(cstrId)

      linearConstraints -= cstrName
      linearConstraintRow -= cstrName
      linearConstraintRow = linearConstraintRow.mapValues(rowId => if(rowId > cstrId) rowId - 1 else rowId)
    }

  /**
   * Adds the given [[LinearConstraint]] to the model
   */
  def addLinearConstraint(linearConstraint: LinearConstraint[I]) = {
    val (varIds, coefs) = linearConstraint.expression.linExpr.coef.map {
      case (vari, coef) => (variableColumn(vari.name), coef)
    }.unzip

    val rowId = solverInterface.addConstraint(linearConstraint.name, coefs.toArray, varIds.toArray, linearConstraint.expression.sense.symbol, -linearConstraint.expression.linExpr.cte)

    register(linearConstraint, rowId)
  }

  /* INDICATOR CONSTRAINTS */
  private var indicatorConstraints = Map[String, IndicatorConstraint[I]]()
  private var linearConstraintsPerIndicatorConstraint = Map[String, Seq[String]]()

  private def register(indicatorConstraint: IndicatorConstraint[I], relatedConstraints: Seq[LinearConstraint[I]]): Unit = {
    setDirty()

    require(!indicatorConstraints.contains(indicatorConstraint.name), s"There exists already an indicator constraint with name ${indicatorConstraint.name}.")

    indicatorConstraints += (indicatorConstraint.name -> indicatorConstraint)
    linearConstraintsPerIndicatorConstraint += (indicatorConstraint.name -> relatedConstraints.map(_.name))
  }

  /**
   * Adds the given [[IndicatorConstraint]] to the model
   */
  def addIndicatorConstraint(indicatorConstraint: IndicatorConstraint[I]) = {
    val constraints = indicatorConstraint.expression.constraintExpressions.zipWithIndex.map { case (cstr, i) =>
      LinearConstraint(s"${indicatorConstraint.name}_${cstr.sense}_$i", cstr)(this)
    }

    register(indicatorConstraint, constraints)
  }


  /* PIECEWISE LINEAR EXPRESSIONS */
  // The linear expression used to represent the piecewise linear expression.
  private var _piecewiseExpr: Map[String, LinearExpression] = Map()
  // The variables used to model the piecewise linear expression as a linear expression.
  private var piecewiseVars: Map[String, Seq[MPVar[MIPSolverInterface]]] = Map()
  // The additional constraints used to model the piecewise linear expression as a linear expression.
  private var piecewiseConstraints: Map[String, Seq[LinearConstraint[MIPSolverInterface]]] = Map()

  /**
   * Returns the [[LinearExpression]] used in the model to represent the piecewise linear expression with the given name.
   */
  def piecewiseExpr(pwleName: String) = _piecewiseExpr(pwleName)

  /**
   * Removes the piecewise linear expression with the given name from the model (if any).
   *
   * It removes all the constraints and variables added to the model to represent the piecewise linear expression.
   *
   * It is not recursive, in case the PiecewiseLinearExpression is a composition of piecewise linear expression:
   *  f(g(x)) with f and g piecewise linear expressions.
   * it removes only the variables and constraints associated to the outer expression (f).
   * The inner expression g should be removed explicitly by a call to [[MPSolver.removePiecewiseLinearExpression]].
   */
  def removePiecewiseLinearExpression(pwleName: String): Unit = {
    _piecewiseExpr -= pwleName
    // Constraints should be removed first in order to be able to remove the variables.
    // Indeed to remove a variable, it should not be present in either the objective or the constraints.
    piecewiseConstraints.get(pwleName).foreach(vOpt => vOpt.foreach(c => this.removeLinearConstraint(c.name)))
    piecewiseConstraints -= pwleName
    piecewiseVars.get(pwleName).foreach(vOpt => vOpt.foreach(v => this.removeVariable(v.name)))
    piecewiseVars -= pwleName
  }

  /* ABSOLUTE VALUE EXPRESSION */
  private var currentAbsExprId: Int = 0
  private def nextAbsExprId: Int = {
    val i = currentAbsExprId
    currentAbsExprId += 1
    i
  }

  /**
   * Adds a new absolute value expression to the solver.
   *
   * |f(x)| = {
   *    -f(x)  if f(x) is <= 0
   *     f(x)  if f(x) is >= 0
   * }
   *
   * The absolute value is modelled as a [[LinearExpression]]
   * by adding additional variables and constraints to the model.
   *
   * The [[LinearExpression]] can be retrieved by calling [[MPSolver.absLinearExpression()]]
   * with the name associated to this absolute value expression.
   *
   * @param linearExpression = f(x) the [[LinearExpression]] whose absolute value is to be computed
   * @param lowerBound the lower bound on f(x).
   * @param upperBound the upper bound on f(x). It should be greater or equal to the lowerBound.
   *
   * @return the name associated to the absolute value expression
   */
  def addAbsExpression[J <: MIPSolverInterface](linearExpression: LinearExpression, lowerBound: Double, upperBound: Double)(implicit ev: MPSolver[I] => MPSolver[J]): String = {
    require(lowerBound <= upperBound, "The lower bound given to an absolute expression should be smaller or equal to the upper bound.")

    setDirty()

    val name = s"abs$nextAbsExprId"

    if(lowerBound == upperBound) {
      _piecewiseExpr += (name -> Const(math.abs(lowerBound)))
    } else if(lowerBound < upperBound && lowerBound <= 0 && upperBound >= 0) {
      implicit val solver = ev(this)

      val xPlus = MPFloatVar(s"${name}_xPlus", lb = 0) // the positive part of the linear expression (it is a positive value)
      val xMinus = MPFloatVar(s"${name}_xMinus", lb = 0) // the negative part of the linear expression (it is a positive value)

      val b = MPBinaryVar(s"${name}_b") // b = 1 if the linear expression is positive, 0 otherwise

      piecewiseVars += (name -> Seq(xPlus, xMinus, b))

      val xDef = LinearConstraint(s"${name}_xDef", linearExpression =:= xPlus - xMinus)
      val xPlusUB = LinearConstraint(s"${name}_xPlusUB", xPlus <:= upperBound * b)
      val xMinusUB = LinearConstraint(s"${name}_xMinusUB", xMinus <:= math.abs(lowerBound) * (1 - b))

      piecewiseConstraints += (name -> Seq(xDef, xPlusUB, xMinusUB))

      // |x| = xPlus + xMinus
      _piecewiseExpr += (name -> (xPlus + xMinus))
    } else if(lowerBound < upperBound && upperBound <= 0) {
      _piecewiseExpr += (name -> -linearExpression)
    } else if(lowerBound < upperBound && lowerBound >= 0) {
      _piecewiseExpr += (name -> linearExpression)
    }

    name
  }

  /**
   * Returns the [[LinearExpression]] used in the model to represent the absolute value expression with the given name.
   */
  def absLinearExpression(absExprName: String) = piecewiseExpr(absExprName)

  /**
   * Removes the absolute value expression with the given name from the model.
   */
  def removeAbsExpression(absExprName: String) = removePiecewiseLinearExpression(absExprName)

  /* SIGN EXPRESSION */
  private var currentSignExprId: Int = 0
  private def nextSignExprId: Int = {
    val i = currentSignExprId
    currentSignExprId += 1
    i
  }

  /**
   * Adds a new sign expression to the solver.
   *
   * sign(x) = {
   *    - 1  if f(x) is < 0
   *      0  if f(x) is 0
   *      1  if f(x) is > 0
   * }
   *
   * The sign function is modelled as a [[LinearExpression]]
   * by adding additional variables and constraints to the model.
   *
   * The [[LinearExpression]] can be retrieved by calling [[MPSolver.signLinearExpression()]]
   * with the name associated to this sign expression.
   *
   * @param linearExpression = f(x) the [[LinearExpression]] whose sign is to be computed
   * @param lowerBound the lower bound on f(x).
   * @param upperBound the upper bound on f(x). It should be greater or equal to the lowerBound.
   * @param eps defines a small interval around f(x) = 0 within which f(x) is considered null.
   *            It should be positive. Default is 1e-5.
   *
   * @return the name associated to the sign expression
   */
  def addSignExpression[J <: MIPSolverInterface](linearExpression: LinearExpression, lowerBound: Double, upperBound: Double, eps: Double = 1e-5)(implicit ev: MPSolver[I] => MPSolver[J]): String = {
    require(lowerBound <= upperBound, "The lower bound given to a sign expression should be smaller or equal to the upper bound.")
    require(eps >= 0, "The eps given to a sign expression should be positive.")

    setDirty()

    val name = s"sign$nextSignExprId"

    if(lowerBound == upperBound) {
      _piecewiseExpr += (name -> Const(math.signum(lowerBound)))
    } else if(lowerBound < upperBound && lowerBound <= 0 && upperBound >= 0) {
      implicit val solver = ev(this)

      val xPlus = MPFloatVar(s"${name}_xPlus") // the positive part of the linear expression (it is a positive value)
      val xMinus = MPFloatVar(s"${name}_xMinus") // the negative part of the linear expression (it is a positive value)
      val xZero = MPFloatVar(s"${name}_xZero") // represents a small interval around zero, within which the linear expression is considered as zero.

      val bPlus = MPBinaryVar(s"${name}_bPlus") // bPlus  = 1 if the linear expression is strictly positive, 0 otherwise
      val bMinus = MPBinaryVar(s"${name}_bMinus") // bMinus = 1 if the linear expression is strictly negative, 0 otherwise
      val bZero = MPBinaryVar(s"${name}_bZero") // bZero  = 1 if the linear expression is around zero,       0 otherwise

      piecewiseVars += (name -> Seq(xPlus, xMinus, xZero, bPlus, bMinus, bZero))

      val xDef = LinearConstraint(s"${name}_xDef", linearExpression =:= xPlus - xMinus + xZero)
      val xPlusLB = LinearConstraint(s"${name}_xPlusLB", xPlus >:= eps * bPlus)
      val xPlusUB = LinearConstraint(s"${name}_xPlusUB", xPlus <:= upperBound * bPlus)
      val xMinusLB = LinearConstraint(s"${name}_xMinusLB", xMinus >:= eps * bMinus)
      val xMinusUB = LinearConstraint(s"${name}_xMinusUB", xMinus <:= math.abs(lowerBound) * bMinus)
      val xZeroLB = LinearConstraint(s"${name}_xZeroLB", xZero >:= -eps * bZero)
      val xZeroUB = LinearConstraint(s"${name}_xZeroUB", xZero <:= eps * bZero)
      val bDef = LinearConstraint(s"${name}_bDef", bPlus + bMinus + bZero =:= 1)

      piecewiseConstraints += (name -> Seq(xDef, xPlusLB, xPlusUB, xMinusLB, xMinusUB, xZeroLB, xZeroUB, bDef))

      // sign(x) = bPlus - bMinus
      //  if x > eps         => bPlus = 1, bMinus = 0 => sign(x) = 1
      //  if x < eps         => bPlus = 0, bMinus = 1 => sign(x) = -1
      //  if -eps < x <- eps => bPlus = 0, bMinus = 0 => sign(x) = 0
      _piecewiseExpr += (name -> (bPlus - bMinus))
    } else if(lowerBound < upperBound && upperBound < 0) {
      _piecewiseExpr += (name -> Const(-1.0))
    } else if(lowerBound < upperBound && lowerBound > 0) {
      _piecewiseExpr += (name -> Const(1.0))
    }

    name
  }

  /**
   * Returns the [[LinearExpression]] used in the model to represent the sign expression with the given name.
   */
  def signLinearExpression(signExprName: String) = piecewiseExpr(signExprName)

  /**
   * Removes the sign expression with the given name from the model.
   */
  def removeSignExpression(signExprName: String) = removePiecewiseLinearExpression(signExprName)


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

  private def asSuccessIfSolFound[B](value: B): Try[B] = endStatus.flatMap { status =>
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


  /* LOGGING */

  /**
   * Saves the problem to the file at the given path in the given format.
   */
  def exportModel(filepath: Path, format: ModelExportFormat): Unit = {
    if(dirty) solverInterface.updateModel()

    solverInterface.exportModel(filepath, format)
  }

  /**
   * Sets the log output of the solver to the given [[LogOutput]]
   */
  def setLogOutput(logOutput: LogOutput): Unit = solverInterface.setLogOutput(logOutput)


  /* INFEASIBILITY ANALYSIS */

  private var infeasibilitiesFound = false

  private def asSuccessIfInfeasFound[B](value: B): Try[B] = {
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

        // linear constraints coming from an indicator constraint are not taken into account
        val linearCstrs = linearConstraints.filterNot { case (name, _) =>
          linearConstraintsPerIndicatorConstraint.values.flatten.toSeq.contains(name)
        }.values.filter(_.infeasible.get).map(_.name).toSeq
        val indicatorCstrs = indicatorConstraints.values.filter(_.infeasible.get).map(_.name).toSeq
        val lowerBounds = variables.values.filter(_.lowerBoundInfeasible.get).map(_.name).toSeq
        val upperBounds = variables.values.filter(_.upperBoundInfeasible.get).map(_.name).toSeq

        Success(InfeasibleSet(linearCstrs ++ indicatorCstrs, lowerBounds, upperBounds))
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
    asSuccessIfInfeasFound(ev(solverInterface).isLinearConstraintInfeasible(linearConstraintRow(cstrName)))

  /**
   * Returns true if the given indicator constraint
   * belongs to the set of constraints making the problem infeasible.
   *
   * An indicator constraints is infeasible as soon as any of its related linear constraints is infeasible.
   */
  def isIndicatorConstraintInfeasible(cstrName: String)(implicit ev: I => InfeasibilityAnalysisInterface): Try[Boolean] =
    asSuccessIfInfeasFound {
      linearConstraintsPerIndicatorConstraint(cstrName).exists { lcName =>
        ev(solverInterface).isLinearConstraintInfeasible(linearConstraintRow(lcName))
      }
    }
}
