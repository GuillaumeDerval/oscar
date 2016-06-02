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

  // originalEnv is the original environment that is passed to the model.
  //   It is this environment that should be released when releasing the solver.
  val (rawSolver, originalEnv) = {
    val env = _env.getOrElse(new GRBEnv())
    (new GRBModel(env), env)
  }

  // The GRBEnv used by this GRBModel, it should be used to modify the solver parameters
  // Note that env is a copy of the originalEnv (see http://www.gurobi.com/documentation/6.5/refman/java_grbmodel_getenv.html)
  // This environment is released when GRBModel is disposed
  val env = rawSolver.getEnv

  private def toGRBLinExpr(coefs: Array[Double], varIds: Array[Int]): GRBLinExpr = {
    updateVars()
    val vars = getVars
    val linExpr = new GRBLinExpr()
    linExpr.addTerms(coefs, varIds map (vars(_)))
    linExpr
  }

  private var nCols = 0
  private var nRows = 0

  def getNumberOfVariables: Int = nCols
  def getNumberOfLinearConstraints: Int = nRows

  def modelName: String = rawSolver.get(GRB.StringAttr.ModelName)
  def modelName_=(value: String) = rawSolver.set(GRB.StringAttr.ModelName, value)

  /* OBJECTIVE */

  private var pendingObj: Option[GRBLinExpr] = None
  private var minimize: Boolean = true
  private def flushPendingObj() = pendingObj = None

  private def getObjective =
    pendingObj.getOrElse(rawSolver.getObjective.asInstanceOf[GRBLinExpr])

  def setObjective(minimize: Boolean, coefs: Array[Double], varIds: Array[Int]): Unit = {
    val obj = toGRBLinExpr(coefs, varIds)
    pendingObj = Some(obj)
    this.minimize = minimize
  }

  def setObjectiveCoefficient(varId: Int, coef: Double): Unit = {
    val variable = getVar(varId)
    val obj = getObjective
    obj.remove(variable)
    obj.addTerm(coef, variable)
    pendingObj = Some(obj)
  }


  /* VARIABLES */

  private def toGRBVarType(integer: Boolean, binary: Boolean): Char = (integer, binary) match {
    case (true, false) => GRB.INTEGER
    case (true, true) => GRB.BINARY
    case (false, false) => GRB.CONTINUOUS
  }

  private var pendingVars: Seq[(Int, GRBVar)] = Seq()
  private def flushPendingVars() = pendingVars = Seq()

  private def getVar(varId: Int): GRBVar =
    pendingVars.find(_._1 == varId).map(_._2).getOrElse(rawSolver.getVar(varId))

  private def getVars =
    rawSolver.getVars ++ pendingVars.sortBy(_._1).map(_._2)

  private def addVariable(name: String, lb: Double, ub: Double,
    objCoef: Option[Double], cstrCoefs: Option[Array[Double]], cstrIds: Option[Array[Int]], integer: Boolean, binary: Boolean): Int = {

    val varId = this.nCols
    val varName = cropName(name, uniqueId = varId.toString)

    // Add the variable to the solver
    val newVar = (objCoef, cstrCoefs, cstrIds) match {
      case (Some(oCoef), Some(cCoefs), Some(cIds)) =>
        rawSolver.addVar(lb, ub, oCoef, toGRBVarType(integer, binary), cIds.map(i => rawSolver.getConstr(i)), cCoefs, varName)
      case (None, None, None) =>
        // Note: actual addition of the variable is delayed until the next updateModel
        rawSolver.addVar(lb, ub, 0.0, toGRBVarType(integer, binary), varName)
      case _ =>
        throw new IllegalArgumentException("Parameters objCoef, cstrCoef, cstrId should all be defined or none.")
    }

    pendingVars = (varId, newVar) +: pendingVars
    this.nCols += 1

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
    updateVars()
    this.nCols -= 1
    rawSolver.remove(getVar(varId))
  }

  private var dirtyVars = false

  def getVariableLowerBound(varId: Int): Double = {
    updateVars()
    getVar(varId).get(GRB.DoubleAttr.LB)
  }
  def setVariableLowerBound(varId: Int, lb: Double) = {
    dirtyVars = true
    getVar(varId).set(GRB.DoubleAttr.LB, lb)
  }

  def getVariableUpperBound(varId: Int): Double = {
    updateVars()
    getVar(varId).get(GRB.DoubleAttr.UB)
  }
  def setVariableUpperBound(varId: Int, ub: Double) = {
    dirtyVars = true
    getVar(varId).set(GRB.DoubleAttr.UB, ub)
  }

  private def getVarType(varId: Int) = {
    updateVars()
    getVar(varId).get(GRB.CharAttr.VType)
  }
  private def setVarType(varId: Int, t: Char) = {
    dirtyVars = true
    getVar(varId).set(GRB.CharAttr.VType, t)
  }

  def isInteger(varId: Int): Boolean = getVarType(varId) == GRB.INTEGER
  def setInteger(varId: Int) = setVarType(varId, GRB.INTEGER)

  def isBinary(varId: Int): Boolean = getVarType(varId) == GRB.BINARY
  def setBinary(varId: Int) = setVarType(varId, GRB.BINARY)

  def isFloat(varId: Int): Boolean = getVarType(varId) == GRB.CONTINUOUS
  def setFloat(varId: Int) = setVarType(varId, GRB.CONTINUOUS)


  /* CONSTRAINTS */

  private var pendingCstrs: Seq[(Int, String, Array[Double], Array[Int], String, Double)] = Seq()
  private def flushPendingCstrs() = pendingCstrs = Seq()

  def getCstr(cstrId: Int) = rawSolver.getConstr(cstrId)

  private def addConstraintToModel(cstrId: Int, name: String, coefs: Array[Double], varIds: Array[Int], sense: String, rhs: Double) = {
    val sen = sense match {
      case "<=" => GRB.LESS_EQUAL
      case ">=" => GRB.GREATER_EQUAL
      case "==" => GRB.EQUAL
      case _ => throw new IllegalArgumentException(s"Unexpected symbol for sense. Found: $sense. Expected: one of <=, == or >=.")
    }

    val cstrExpr = toGRBLinExpr(coefs, varIds)
    rawSolver.addConstr(cstrExpr, sen, rhs, name)
  }

  def addConstraint(name: String, coefs: Array[Double], varIds: Array[Int], sense: String, rhs: Double): Int = {
    val cstrId = this.nRows
    pendingCstrs = (cstrId, name, coefs, varIds, sense, rhs) +: pendingCstrs
    this.nRows += 1
    cstrId
  }

  def removeConstraint(cstrId: Int): Unit = {
    this.nRows -= 1
    if (pendingCstrs.exists(c => c._1 == cstrId)) {
      pendingCstrs = pendingCstrs.flatMap { c =>
        if (c._1 > cstrId) Some((c._1 - 1, c._2, c._3, c._4, c._5, c._6))
        else if (c._1 < cstrId) Some(c)
        else None
      }
    } else rawSolver.remove(getCstr(cstrId))
  }

  def setConstraintCoefficient(cstrId: Int, varId: Int, coef: Double): Unit = {
    pendingCstrs.find(_._1 == cstrId) match {
      case Some((_, _, coefs, _, _, _)) =>
        val newCoefs = coefs.zipWithIndex.map { case (c, i) =>
          if(i == varId) coef else c
        }
        pendingCstrs = pendingCstrs.map { c =>
          if (c._1 == cstrId) (c._1, c._2, newCoefs, c._4, c._5, c._6)
          else c
        }
      case None => rawSolver.chgCoeff(getCstr(cstrId), getVar(varId), coef)
    }
  }

  def setConstraintRightHandSide(cstrId: Int, rhs: Double): Unit = {
    pendingCstrs.find(_._1 == cstrId) match {
      case Some((_, _, _, _, _, oldRhs)) =>
        pendingCstrs = pendingCstrs.map { c =>
          if (c._1 == cstrId) (c._1, c._2, c._3, c._4, c._5, rhs)
          else c
        }
      case None => getCstr(cstrId).set(GRB.DoubleAttr.RHS, rhs)
    }
  }


  /* SOLVE */

  def updateVars() =
    if(pendingVars.nonEmpty || dirtyVars) {
      rawSolver.update()
      flushPendingVars()
    }

  def updateModel() = {
    updateVars()

    pendingObj.foreach(obj => rawSolver.setObjective(obj, if(this.minimize) GRB.MINIMIZE else GRB.MAXIMIZE))
    flushPendingObj()

    pendingCstrs.sortBy(_._1).foreach { case (cstrId, name, coefs, varIds, sense, rhs) =>
      addConstraintToModel(cstrId, name, coefs, varIds, sense, rhs)
    }
    flushPendingCstrs()

    rawSolver.update()

    // verify that the raw model contains the correct number of variables and constraints
    assert(rawSolver.get(GRB.IntAttr.NumVars) == getNumberOfVariables,
      s"$solverName: the number of variables contained by the raw solver does not correspond to the number of variables added.")
    assert(rawSolver.get(GRB.IntAttr.NumConstrs) == getNumberOfLinearConstraints,
      s"$solverName: the number of constraints contained by the raw solver does not correspond to the number of constraints added.")
  }

  private def verifySolutionExists = if(rawSolver.get(GRB.IntAttr.SolCount) > 0) SolutionFound else NoSolutionFound

  def endStatus: EndStatus =
    rawSolver.get(GRB.IntAttr.Status) match {
      case GRB.LOADED => throw NotSolvedYetException
      case GRB.INPROGRESS => throw NotSolvedYetException
      case GRB.OPTIMAL => SolutionFound
      case GRB.SUBOPTIMAL => SolutionFound
      case GRB.INFEASIBLE => Infeasible
      case GRB.UNBOUNDED => Unbounded
      case GRB.INF_OR_UNBD => InfeasibleOrUnbounded
      case GRB.CUTOFF => NoSolutionFound
      case GRB.TIME_LIMIT => verifySolutionExists
      case GRB.SOLUTION_LIMIT => verifySolutionExists
      case GRB.ITERATION_LIMIT => verifySolutionExists
      case GRB.NODE_LIMIT => verifySolutionExists
      case GRB.INTERRUPTED => verifySolutionExists
      case GRB.NUMERIC => verifySolutionExists
    }

  def hasSolution: Boolean =
    try {
      endStatus == SolutionFound
    } catch {
      case NotSolvedYetException => false
    }

  def solutionQuality: SolutionQuality =
    endStatus match {
      case SolutionFound =>
        rawSolver.get(GRB.IntAttr.Status) match {
          case GRB.OPTIMAL => Optimal
          case _ => Suboptimal
        }
      case _ => throw NoSolutionFoundException(endStatus)
    }


  def objectiveValue: Double = rawSolver.get(GRB.DoubleAttr.ObjVal)

  def objectiveBound: Double = rawSolver.get(GRB.DoubleAttr.ObjBound)

  def solution: Array[Double] = rawSolver.get(GRB.DoubleAttr.X, rawSolver.getVars)

  private[gurobi] var aborted: Boolean = false
  rawSolver.setCallback(new GurobiAborter(this))

  def solve: EndStatus = {
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
      originalEnv.release()
      originalEnv.dispose()
    }
    super.release()
  }


  /* LOGGING */

  def exportModel(filePath: java.nio.file.Path): Unit = {
    val format = getExtension(filePath)
    require(
      format.equals("lp") || format.equals("mps"),
      s"Unexpected file extension: $format"
    )

    rawSolver.write(filePath.toString)
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
      case _ => throw new IllegalArgumentException(s"Unrecognised log output $logOutput")
    }
  }

  /* CONFIGURATION */

  def configure(absPath: Path) = env.readParams(absPath.toString)

  def setTimeout(t: Long): Unit = env.set(GRB.DoubleParam.TimeLimit, t.toDouble)


  /* CALLBACK */

  // All progress events need to be covered in the callback. It is a global callback, responsible
  // for logging everything.
  // Example: http://www.gurobi.com/documentation/6.5/examples/callback_java.html
  def addGapCallback() = throw new NotImplementedError("Gap callback is not implemented for Gurobi")
  def getCurrentGap: Option[Double] = None
}
