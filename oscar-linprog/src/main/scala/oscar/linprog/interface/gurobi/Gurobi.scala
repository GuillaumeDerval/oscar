/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.linprog.interface.gurobi

import java.nio.file.Path

import gurobi._
import oscar.linprog._
import oscar.linprog.enums._
import oscar.linprog.interface.{MIPSolverInterface, MPSolverInterface}

/**
 * Interface for solver [[gurobi.GRBModel]]
 *
 * @author acrucifix acr@n-side.com
 */
class Gurobi(_env: Option[GRBEnv] = None) extends MPSolverInterface with MIPSolverInterface {

  type Solver = GRBModel

  val solverName = "gurobi"

  private val env = _env match {
    case Some(e) => e
    case None => new GRBEnv()
  }

  val rawSolver = new GRBModel(env)

  private def toGRBLinExpr(coefs: Array[Double], varIds: Array[Int]): GRBLinExpr = {
    val vars = rawSolver.getVars
    val linExpr = new GRBLinExpr()
    linExpr.addTerms(coefs, varIds map (vars(_)))
    linExpr
  }

  def getNumberOfVariables: Int = rawSolver.get(GRB.IntAttr.NumVars)
  def getNumberOfLinearConstraints: Int = rawSolver.get(GRB.IntAttr.NumConstrs)

  def modelName: String = rawSolver.get(GRB.StringAttr.ModelName)
  def modelName_=(value: String) = rawSolver.set(GRB.StringAttr.ModelName, value)


  /* OBJECTIVE */

  def setObjective(minimize: Boolean, coefs: Array[Double], varIds: Array[Int]): Unit =
    rawSolver.setObjective(toGRBLinExpr(coefs, varIds), if(minimize) 1 else -1)

  def setObjectiveCoefficient(varId: Int, coef: Double): Unit = {
    val variable = rawSolver.getVar(varId)
    val obj = rawSolver.getObjective.asInstanceOf[GRBLinExpr]
    obj.remove(variable)
    obj.addTerm(coef, variable)
    rawSolver.setObjective(obj)
  }


  /* VARIABLES */

  private def toGRBVarType(integer: Boolean, binary: Boolean): Char = (integer, binary) match {
    case (true, false) => GRB.INTEGER
    case (true, true) => GRB.BINARY
    case (false, false) => GRB.CONTINUOUS
  }

  private def addVariable(name: String, lb: Double, ub: Double,
    objCoef: Option[Double], cstrCoefs: Option[Array[Double]], cstrIds: Option[Array[Int]], integer: Boolean, binary: Boolean): Int = {
    val varId = (objCoef, cstrCoefs, cstrIds) match {
      case (Some(oCoef), Some(cCoefs), Some(cIds)) =>
        val varId = getNumberOfVariables
        rawSolver.addVar(lb, ub, oCoef, toGRBVarType(integer, binary), cIds.map(i => rawSolver.getConstr(i)), cCoefs, name)
        varId
      case (None, None, None) =>
        val varId = getNumberOfVariables
        rawSolver.addVar(lb, ub, 0.0, toGRBVarType(integer, binary), name)
        varId
      case _ =>
        throw new IllegalArgumentException("Parameters objCoef, cstrCoef, cstrId should all be defined or none.")
    }
    // Important ! the model should be immediately updated so that
    //   the variable can be used immediately after
    //   the number of variables as seen by getNumberOfVariables is correct which is important for indexing
    rawSolver.update()

    varId
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

  def removeVariable(varId: Int): Unit = {
    rawSolver.remove(rawSolver.getVar(varId))
    // Important ! the model should be immediately updated so that
    //   the number of variables as seen by getNumberOfVariables is correct which is important for indexing
    rawSolver.update()
  }

  def getVariableLowerBound(varId: Int): Double = rawSolver.getVar(varId).get(GRB.DoubleAttr.LB)
  def setVariableLowerBound(varId: Int, lb: Double) = {
    rawSolver.getVar(varId).set(GRB.DoubleAttr.LB, lb)
    // Important ! the model should be immediately updated so that
    //   the type of the variable is correctly reflected by a call to getVariableLowerBound
    rawSolver.update()
  }

  def getVariableUpperBound(varId: Int): Double = rawSolver.getVar(varId).get(GRB.DoubleAttr.UB)
  def setVariableUpperBound(varId: Int, ub: Double) = {
    rawSolver.getVar(varId).set(GRB.DoubleAttr.UB, ub)
    // Important ! the model should be immediately updated so that
    //   the type of the variable is correctly reflected by a call to getVariableUpperBound
    rawSolver.update()
  }

  private def getVarType(varId: Int) = rawSolver.getVar(varId).get(GRB.CharAttr.VType)
  private def setVarType(varId: Int, t: Char) = {
    rawSolver.getVar(varId).set(GRB.CharAttr.VType, t)
    // Important ! the model should be immediately updated so that
    //   the type of the variable is correctly reflected by a call to getVarType
    rawSolver.update()
  }

  def isInteger(varId: Int): Boolean = getVarType(varId) == GRB.INTEGER
  def setInteger(varId: Int) = {
    setVarType(varId, GRB.INTEGER)
    // Important ! the model should be immediately updated so that
    //   the type of the variable is correctly reflected by a call to isInteger
    rawSolver.update()
  }

  def isBinary(varId: Int): Boolean = getVarType(varId) == GRB.BINARY
  def setBinary(varId: Int) = {
    setVarType(varId, GRB.BINARY)
    // Important ! the model should be immediately updated so that
    //   the type of the variable is correctly reflected by a call to isBinary
    rawSolver.update()
  }

  def isFloat(varId: Int): Boolean = getVarType(varId) == GRB.CONTINUOUS
  def setFloat(varId: Int) = {
    setVarType(varId, GRB.CONTINUOUS)
    // Important ! the model should be immediately updated so that
    //   the type of the variable is correctly reflected by a call to isFloat
    rawSolver.update()
  }


  /* CONSTRAINTS */

  def addConstraint(name: String, coefs: Array[Double], varIds: Array[Int], sense: String, rhs: Double): Int = {
    val cstrId = getNumberOfLinearConstraints

    val GRBSense =
      sense match {
        case "<=" => GRB.LESS_EQUAL
        case ">=" => GRB.GREATER_EQUAL
        case "==" => GRB.EQUAL
      }

    val cstrExpr = toGRBLinExpr(coefs, varIds)

    rawSolver.addConstr(cstrExpr, GRBSense, rhs, name)

    // Important ! the model should be immediately updated so that
    //   the number of constraints as seen by getNumberOfLinearConstraints is correct which is important for indexing
    rawSolver.update()

    cstrId
  }

  def removeConstraint(cstrId: Int): Unit = {
    rawSolver.remove(rawSolver.getConstr(cstrId))
    // Important ! the model should be immediately updated so that
    //   the number of constraints as seen by getNumberOfLinearConstraints is correct which is important for indexing
    rawSolver.update()
  }

  def setConstraintCoefficient(cstrId: Int, varId: Int, coef: Double): Unit =
    rawSolver.chgCoeff(rawSolver.getConstr(cstrId), rawSolver.getVar(varId), coef)

  def setConstraintRightHandSide(cstrId: Int, rhs: Double): Unit =
    rawSolver.getConstr(cstrId).set(GRB.DoubleAttr.RHS, rhs)


  /* SOLVE */

  def updateModel() = rawSolver.update()

  def endStatus: EndStatus =
    rawSolver.get(GRB.IntAttr.Status) match {
      case GRB.OPTIMAL => SolutionFound
      case GRB.SUBOPTIMAL => SolutionFound
      case GRB.INFEASIBLE => Infeasible
      case GRB.UNBOUNDED => Unbounded
      case GRB.INF_OR_UNBD => InfeasibleOrUnbounded
      case GRB.LOADED => throw NotSolvedYetException
      case _ => Warning
    }

  def hasSolution: Boolean =
    try {
      endStatus == SolutionFound
    } catch {
      case NotSolvedYetException => false
    }

  def solutionQuality: SolutionQuality =
    rawSolver.get(GRB.IntAttr.Status) match {
      case GRB.OPTIMAL => Optimal
      case GRB.SUBOPTIMAL => Suboptimal
      case _ => throw NoSolutionFoundException(endStatus)
    }

  def objectiveValue: Double = rawSolver.get(GRB.DoubleAttr.ObjVal)

  def objectiveBound: Double = rawSolver.get(GRB.DoubleAttr.ObjBound)

  def solution: Array[Double] =rawSolver.getVars.map(v => v.get(GRB.DoubleAttr.X))

  private[gurobi] var aborted: Boolean = false
  rawSolver.setCallback(new GurobiAborter(this))

  def solve: EndStatus = {
    updateModel()
    aborted = false

    rawSolver.optimize()

    // In case presolve declared the status InfeasibleOrUnbounded,
    // deactivate presolve and solve again to find out.
    if(rawSolver.get(GRB.IntAttr.Status) == GRB.INF_OR_UNBD) {
      val presolve = env.get(GRB.IntParam.Presolve)
      env.set(GRB.IntParam.Presolve, 0)
      rawSolver.optimize()
      // reset previous value for presolve
      env.set(GRB.IntParam.Presolve, presolve)
    }

    endStatus
  }

  def abort(): Unit = aborted = true

  override def release(): Unit = {
    rawSolver.dispose()
    // If the environment was self-made, release it also.
    // Otherwise, it is the responsibility of the user to release it.
    if(_env.isEmpty) {
      env.release()
      env.dispose()
    }
    super.release()
  }


  /* LOGGING */

  /**
   * Gurobi's export file handling is a little different. The format is defined by the fileName passed to model.write:
   *  - for LP  the file should end with .lp
   *  - for MPS the file should end with .mps
   *
   * Therefore, the file extension is checked against the given format to make sure it matches.
   */
  def exportModel(filePath: java.nio.file.Path, format: ModelExportFormat): Unit = {
    require(format.checkExtension(filePath), s"Unexpected file extension (${filePath.extension}) for the given model export format ($format)")

    format match {
      case MPS => rawSolver.write(filePath.toString)
      case LP => rawSolver.write(filePath.toString)
      case _  => println(s"Unrecognised export format $format")
    }
  }

  override def setLogOutput(logOutput: LogOutput): Unit = {
    super.setLogOutput(logOutput)

    logOutput match {
      case DisabledLogOutput =>
        env.set(GRB.IntParam.OutputFlag, 0)
        env.set(GRB.IntParam.LogToConsole, 0)
        env.set(GRB.StringParam.LogFile, "")
      case StandardLogOutput =>
        env.set(GRB.IntParam.OutputFlag, 1)
        env.set(GRB.IntParam.LogToConsole, 1)
        env.set(GRB.StringParam.LogFile, "")
      case FileLogOutput(path) =>
        env.set(GRB.IntParam.OutputFlag, 1)
        env.set(GRB.IntParam.LogToConsole, 0)
        env.set(GRB.StringParam.LogFile, path.toString)
      case _ => println(s"Unrecognised log output $logOutput")
    }
  }

  /* CONFIGURATION */

  def configure(absPath: Path) = env.readParams(absPath.toString)

  def setTimeout(t: Long): Unit = env.set(GRB.DoubleParam.TimeLimit, t.toDouble)
}
