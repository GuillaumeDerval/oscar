package oscar.linprog

import java.nio.file.Path

import lpsolve.LpSolve
import oscar.algebra._

/**
  * [[SolverInterface]] for using LPSolve to solve linear mathematical [[Model]]s.
  */
object LPSolve extends SolverInterface[Linear, Linear, Double]{
  override def run(model: Model[Linear, Linear, Double],config: Option[Path] = None) = new LPSolveRun(config)(model)
}

/**
  * [[SolverRun]]s using LPSolve to solve [[Model]]s.
  * @param config path to an optional config file to configure LPSolve
  * @param model the [[Model]] to solve with this [[SolverRun]]
  */
class LPSolveRun(config: Option[Path] = None)(private val model: Model[Linear, Linear, Double]) extends SolverRun[Linear, Linear, Double] {


  // CPlex solver
  val rawSolver = LpSolve.makeLp(0, model.variables.size)

  // Abortion management
  class LPSolveAborter extends lpsolve.AbortListener {
    def abortfunc(problem: lpsolve.LpSolve, userhandle: Any): Boolean = aborted
  }

  private var aborted = false
  override def abort {aborted = true}

  rawSolver.putAbortfunc(new LPSolveAborter, null)

  // Release all resources
  def release: Unit ={
    rawSolver.deleteLp()
  }

  //Configure solver
  config match {
    case Some(path) => rawSolver.readParams(path.toString, "[Default]")
    case _ =>
  }

  // Create the CPlex variables
  val cplexVars = {
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

  private def transform(expression: NormalizedExpression[Linear,Double])(f: (Int, Array[Double], Array[Int],Double) => Unit) = {
    val (termsWithVar, constantTerms) = expression.terms.partition(_.terms.nonEmpty)
    f(termsWithVar.size,
      termsWithVar.map{_.coef.d}.toArray,
      termsWithVar.map{_.terms.head.asInstanceOf[Var[Double]].id + 1}.toArray,
      -constantTerms.map(_.coef.d).sum
    )
  }

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

  override def setInteger(v: Var[Double]) = rawSolver.setInt(v.id + 1, true)

  override def setContinuous(vd: Var[Double]) = {
    rawSolver.setInt(vd.id + 1, false)
    rawSolver.setBinary(vd.id + 1, false)
  }

  // Add all constraints to the CPlex model
  for (s <- model.constraints; c <- s.equations) {
    val sen = c.sense match {
      case LQ => LpSolve.LE
      case EQ => LpSolve.EQ
      case GQ => LpSolve.GE
    }

    transform(c.expr)(rawSolver.addConstraintex(_,_,_, sen, _))
  }

  // set the objective
  setObjective(model.objective)


  def solve: ModelStatus[Linear, Linear, Double] = {

    aborted = false

    rawSolver.solve match {
      case LpSolve.OPTIMAL =>
        AOptimal(new Solution[Double](rawSolver.getPtrVariables))
      case LpSolve.SUBOPTIMAL =>
        AFeasible(new Solution[Double](rawSolver.getPtrVariables))
      case LpSolve.INFEASIBLE =>
        AInfeasible()
      case LpSolve.UNBOUNDED =>
        AUnbounded()
      case _ =>
       AWarning()
    }
  }

  override def setLowerBound(v: Var[Double], d: Double): Unit = {
    rawSolver.setLowbo(v.id + 1, d)
  }

  override def setUpperBound(v: Var[Double], d: Double): Unit = {
    rawSolver.setUpbo(v.id + 1, d)
  }
}
