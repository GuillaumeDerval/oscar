package oscar.linprog.lp_solve

import java.nio.file.Path

import oscar.algebra._

/**
 * [[SolverInterface]] for using LPSolve to solve linear mathematical [[Model]]s.
 */
object LPSolve extends SolverInterface[Linear, Linear, Double]("lp_solve"){
  override def run(model: Model[Linear, Linear, Double],config: Option[Path] = None): LPSolveRun =
    new LPSolveRun(config)(model)
}

/**
 * [[SolverRun]]s using LPSolve to solve [[Model]]s.
 *
 * @param config path to an optional config file to configure LPSolve
 * @param model the [[Model]] to solve with this [[SolverRun]]
 */
class LPSolveRun(config: Option[Path] = None)(private val model: Model[Linear, Linear, Double]) extends SolverRun[Linear, Linear, Double] {
  type Solver = lpsolve.LpSolve

  val rawSolver = lpsolve.LpSolve.makeLp(0, model.variables.size)

  override def release(): Unit = { rawSolver.deleteLp() }

  /* --- Configuration --- */

  config.foreach(path => rawSolver.readParams(path.toString, "[Default]"))

  /* --- Abortion management -- */

  private var aborted = false

  private class LPSolveAborter extends lpsolve.AbortListener {
    def abortfunc(problem: lpsolve.LpSolve, userhandle: Any): Boolean = aborted
  }
  rawSolver.putAbortfunc(new LPSolveAborter, null)

  override def abort(): Unit = {aborted = true}

  /* --- Variables --- */

  // Creates the raw solver variables
  private val solverVars = {
    for (vd <- model.variables) yield {
      rawSolver.setColName(vd.id + 1, vd.name)
      rawSolver.setLowbo(vd.id + 1, vd.lowerBound)
      rawSolver.setUpbo(vd.id + 1, vd.upperBound)
      vd match {
        case VarBinary(name) =>
          rawSolver.setBinary(vd.id + 1, true)
        case VarInt(name, lb, ub) =>
          setInteger(vd)
        case VarNumerical(name, lb, ub) =>
          setContinuous(vd)
      }
    }
  }.toArray

  override def setLowerBound(v: Var[Double], d: Double): Unit = {
    rawSolver.setLowbo(v.id + 1, d)
  }

  override def setUpperBound(v: Var[Double], d: Double): Unit = {
    rawSolver.setUpbo(v.id + 1, d)
  }

  override def setInteger(v: Var[Double]): Unit = rawSolver.setInt(v.id + 1, true)

  override def setContinuous(vd: Var[Double]): Unit = {
    rawSolver.setInt(vd.id + 1, false)
    rawSolver.setBinary(vd.id + 1, false)
  }

  /* --- Constraints --- */

  // Transform oscar.algebra.Expression into lpsolve expression
  private def transform(expression: NormalizedExpression[Linear,Double])(f: (Int, Array[Double], Array[Int],Double) => Unit) = {
    val (termsWithVar, constantTerms) = expression.terms.partition(_.vars.nonEmpty)
    f(termsWithVar.size,
      termsWithVar.map{_.coef.d}.toArray,
      termsWithVar.map{_.vars.head.asInstanceOf[Var[Double]].id + 1}.toArray,
      -constantTerms.map(_.coef.d).sum
    )
  }

  // Add all constraints to the solver
  for {
    s <- model.constraints
    c <- s.equations
  } {
    val sen = c.sense match {
      case LQ => lpsolve.LpSolve.LE
      case EQ => lpsolve.LpSolve.EQ
      case GQ => lpsolve.LpSolve.GE
    }

    transform(c.expr)(rawSolver.addConstraintex(_,_,_, sen, _))
  }

  /* --- Objective --- */

  override def setObjective(o: Objective[Linear, Double]): Unit = {

    o.expression.terms.map{_.coef.d}

    transform(o.expression){
      case (a,b,c,_) => rawSolver.setObjFnex(a,b,c)
    }

    o match {
      case _: Maximize[Linear, Double] => rawSolver.setMaxim()
      case _: Minimize[Linear, Double] => rawSolver.setMinim()
    }
  }

  // set the objective
  setObjective(model.objective)

  /* --- Solve --- */

  override def solve: SolveResult[Linear, Linear, Double] = {
    aborted = false

    rawSolver.solve match {
      case lpsolve.LpSolve.OPTIMAL =>
        Optimal(new Solution[Double](rawSolver.getPtrVariables))
      case lpsolve.LpSolve.SUBOPTIMAL =>
        Feasible(new Solution[Double](rawSolver.getPtrVariables))
      case lpsolve.LpSolve.INFEASIBLE =>
        Infeasible()
      case lpsolve.LpSolve.UNBOUNDED =>
        Unbounded()
      case _ =>
        Warning()
    }
  }
}
