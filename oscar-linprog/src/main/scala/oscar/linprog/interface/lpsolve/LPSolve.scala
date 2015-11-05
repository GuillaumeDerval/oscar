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

package oscar.linprog.interface.lpsolve

import java.nio.file.Path

import _root_.lpsolve.LpSolve
import oscar.linprog.enums._
import oscar.linprog.interface._

/**
 * Interface for solver [[LpSolve]]
 *
 * @author acrucifix acr@n-side.com
 */
class LPSolve extends MPSolverInterface with MIPSolverInterface {
  // NOTE:
  // variables (columns) and constraints (rows) indices of LpSolve are 1 based

  type Solver = LpSolve

  val solverName = "lp_solve"

  val rawSolver = LpSolve.makeLp(0, 0)

  // !!! BEWARE: DO NOT UNCOMMENT THE FOLLOWING LINE,
  // current versions of LpSolve behaves unpredictably when changing the value of infinity.
  // rawSolver.setInfinite(Double.PositiveInfinity) // infinity in OscaR is represented by Double.PositiveInfinity

  private var nCols = 0
  private var nRows = 0

  def getNumberOfVariables: Int = nCols
  def getNumberOfLinearConstraints: Int = nRows

  def modelName = rawSolver.getLpName
  def modelName_=(value: String) = rawSolver.setLpName(value)


  /* OBJECTIVE */

  private var pendingObj: Option[(Boolean, Array[Double], Array[Int])] = None
  private def flushPendingObj() = pendingObj = None

  def setObjective(minimize: Boolean, coefs: Array[Double], varIds: Array[Int]): Unit =
    pendingObj = Some((minimize, coefs, varIds))

  def setObjectiveCoefficient(varId: Int, coef: Double): Unit = rawSolver.setObj(varId + 1, coef)


  /* VARIABLES */

  private var pendingVars: Seq[(Int, String, Double, Double, Boolean, Boolean)] = Seq()
  private def flushPendingVars() = pendingVars = Seq()

  private def setVarProperties(varId: Int, name: String, lb: Double, ub: Double, integer: Boolean, binary: Boolean) = {
    // NOTE: do not swap the next two lines, the name of the variable should be set before setting any property.
    rawSolver.setColName(varId + 1, name)
    setVariableLowerBound(varId, lb)
    setVariableUpperBound(varId, ub)
    (integer, binary) match {
      case (true, false) => setInteger(varId)
      case (true, true) => setBinary(varId)
      case (false, false) => setFloat(varId)
    }
  }

  private def addVariable(name: String, lb: Double, ub: Double,
    objCoef: Option[Double], cstrCoefs: Option[Array[Double]], cstrIds: Option[Array[Int]], integer: Boolean, binary: Boolean): Int =
    (objCoef, cstrCoefs, cstrIds) match {
      case (Some(oCoef), Some(cCoefs), Some(cIds)) =>
        // first arg is the length of the arrays
        // second arg is the array of the coefficients of the constraints (coef in position 0 is the coef for the objective)
        // third arg is the row numbers of the constraints that should be updated (row 0 is the objective)
        rawSolver.addColumnex(cCoefs.length + 1, oCoef +: cCoefs, 0 +: cIds.map(_ + 1))
        val varId = this.nCols
        setVarProperties(varId, name, lb, ub, integer, binary)
        this.nCols += 1
        varId
      case (None, None, None) =>
        // Note: actual addition of the variable is delayed until the next updateModel
        val varId = this.nCols
        pendingVars = (varId, name, lb, ub, integer, binary) +: pendingVars
        this.nCols += 1
        varId
      case _ =>
        throw new IllegalArgumentException("Parameters objCoef, cstrCoef, cstrId should all be defined or none.")
    }

  def addVariable(name: String, lb: Double = -Double.MaxValue, ub: Double = Double.MaxValue,
    objCoef: Option[Double] = None, cstrCoefs: Option[Array[Double]] = None, cstrIds: Option[Array[Int]] = None) =
    addVariable(name, lb, ub, objCoef, cstrCoefs, cstrIds, integer = false, binary = false)

  def addIntegerVariable(name: String, from: Int, to: Int,
    objCoef: Option[Double] = None, cstrCoefs: Option[Array[Double]] = None, cstrIds: Option[Array[Int]] = None): Int =
    addVariable(name, from.toDouble, to.toDouble, objCoef, cstrCoefs, cstrIds, integer = true, binary = false)

  def addBinaryVariable(name: String,
    objCoef: Option[Double] = None, cstrCoefs: Option[Array[Double]] = None, cstrIds: Option[Array[Int]] = None): Int =
    addVariable(name, 0.0, 1.0, objCoef, cstrCoefs, cstrIds, integer = true, binary = true)

  def getVariableLowerBound(varId: Int): Double = rawSolver.getLowbo(varId + 1)
  def setVariableLowerBound(varId: Int, lb: Double) = rawSolver.setLowbo(varId + 1, lb)

  def getVariableUpperBound(varId: Int): Double = rawSolver.getUpbo(varId + 1)
  def setVariableUpperBound(varId: Int, ub: Double) = rawSolver.setUpbo(varId + 1, ub)

  def isInteger(varId: Int): Boolean = rawSolver.isInt(varId + 1)
  def setInteger(varId: Int) = rawSolver.setInt(varId + 1, true)

  def isBinary(varId: Int): Boolean = rawSolver.isBinary(varId + 1)
  def setBinary(varId: Int) = rawSolver.setBinary(varId + 1, true)

  def isFloat(varId: Int): Boolean = !isInteger(varId) && !isBinary(varId)
  def setFloat(varId: Int) = {
    if (isInteger(varId)) rawSolver.setInt(varId + 1, false)
    if (isBinary(varId)) rawSolver.setBinary(varId + 1, false)
  }


  /* CONSTRAINTS */

  private var pendingCstrs: Seq[(Int, String, Array[Double], Array[Int], String, Double)] = Seq()
  private def flushPendingCstrs() = pendingCstrs = Seq()

  private def addConstraintToModel(cstrId: Int, name: String, coefs: Array[Double], varIds: Array[Int], sense: String, rhs: Double) = {
    val sen = sense match {
      case "<=" => LpSolve.LE
      case "==" => LpSolve.EQ
      case ">=" => LpSolve.GE
      case _ => throw new IllegalArgumentException(s"Unexpected symbol for sense. Found: $sense. Expected: one of <=, == or >=.")
    }

    rawSolver.addConstraintex(coefs.length, coefs, varIds.map(_ + 1), sen, rhs)
    rawSolver.setRowName(cstrId, name)
  }

  def addConstraint(name: String, coefs: Array[Double], varIds: Array[Int], sense: String, rhs: Double): Int = {
    val cstrId = this.nRows
    pendingCstrs = (cstrId, name, coefs, varIds, sense, rhs) +: pendingCstrs
    this.nRows += 1
    cstrId
  }

  def setConstraintCoefficient(cstrId: Int, varId: Int, coef: Double): Unit = rawSolver.setMat(cstrId + 1, varId + 1, coef)

  def setConstraintRightHandSide(cstrId: Int, rhs: Double): Unit = rawSolver.setRh(cstrId + 1, rhs)


  /* SOLVE */

  def updateModel() = {
    rawSolver.resizeLp(nRows, nCols)

    // add the pending vars
    pendingVars.foreach { case (varId, name, lb, ub, integer, binary) => setVarProperties(varId, name, lb, ub, integer, binary) }
    flushPendingVars()

    // add the pending objective
    pendingObj.foreach { case (min, oCoef, oVarId) =>
      rawSolver.setObjFnex(oCoef.length, oCoef, oVarId.map(_ + 1))
      if(min) rawSolver.setMinim() else rawSolver.setMaxim()
    }
    flushPendingObj()

    // add the pending constraints
    pendingCstrs.sortBy {
      case (cstrId, _, _, _, _, _) => cstrId
    }.foreach {
      case (cstrId, name, coefs, varIds, sense, rhs) => addConstraintToModel(cstrId, name, coefs, varIds, sense, rhs)
    }
    flushPendingCstrs()

    // verify that the raw model contains the correct number of variables and constraints
    assert(rawSolver.getNorigColumns == getNumberOfVariables,
      s"$solverName: the number of variables contained by the raw solver does not correspond to the number of variables added.")
    assert(rawSolver.getNorigRows == getNumberOfLinearConstraints,
      s"$solverName: the number of constraints contained by the raw solver does not correspond to the number of constraints added.")
  }

  def exportModel(filepath: Path, format: ModelExportFormat): Unit =
    format match {
      case LP => rawSolver.writeLp(filepath.toString) // Note: this is lp_solve's own lp format which is different from CPLEX's one.
      case MPS => rawSolver.writeFreeMps(filepath.toString)
      case _ => println(s"Unrecognised export format $format")
    }

  private var _endStatus: Option[EndStatus] = None

  def endStatus: EndStatus = _endStatus match {
    case Some(es) => es
    case None => throw NotSolvedYetException
  }

  def hasSolution: Boolean = _endStatus.isDefined && endStatus == SolutionFound

  private var _solutionQuality: Option[SolutionQuality] = None

  def solutionQuality: SolutionQuality = _solutionQuality match {
    case Some(sq) => sq
    case None => if(_endStatus.isDefined) throw NoSolutionFoundException(endStatus) else throw NotSolvedYetException
  }

  def objectiveValue: Double = rawSolver.getObjective

  def objectiveBound: Double = rawSolver.getObjBound

  def solution: Array[Double] = rawSolver.getPtrVariables

  private[lpsolve] var aborted: Boolean = false
  rawSolver.putAbortfunc(new LPSolveAborter(this), this)

  def solve: EndStatus = {
    updateModel()
    aborted = false

    val status = rawSolver.solve

    status match {
      case LpSolve.OPTIMAL =>
        _endStatus = Some(SolutionFound)
        _solutionQuality = Some(Optimal)
      case LpSolve.SUBOPTIMAL =>
        _endStatus = Some(SolutionFound)
        _solutionQuality = Some(Suboptimal)
      case LpSolve.INFEASIBLE =>
        _endStatus = Some(Infeasible)
      case LpSolve.UNBOUNDED =>
        _endStatus = Some(Unbounded)
      case LpSolve.USERABORT =>
        _endStatus = Some(NoSolutionFound)
      case LpSolve.TIMEOUT =>
        _endStatus = Some(NoSolutionFound)
      case _ =>
        _endStatus = Some(Warning)
    }

    endStatus
  }

  def abort(): Unit = aborted = true

  var _released = false

  def release(): Unit = {
    _released = true
    rawSolver.deleteLp()
  }

  def released: Boolean = _released


  /* CONFIGURATION */

  def configure(absPath: Path) = rawSolver.readParams(absPath.toString, "[Default]")

  def setTimeout(nSeconds: Long) = rawSolver.setTimeout(nSeconds)
}
