package oscar.modeling.models.lp

import oscar.algebra.LinearExpression
import oscar.linprog.interface.gurobi.GurobiLib
import oscar.linprog.interface.lpsolve.LPSolveLib
import oscar.linprog.interface.{MPSolverInterface, MPSolverLib}
import oscar.linprog.modeling.MPSolver
import oscar.modeling.algebra.bool.{BoolExpression, EqFloat, GrEqFloat, LrEqFloat}
import oscar.modeling.algebra.floating._
import oscar.modeling.constraints.{Constraint, ExpressionConstraint}
import oscar.modeling.models._
import oscar.modeling.vars.FloatVar
import oscar.modeling.vars.mip.MIPFloatVar
import oscar.modeling.vars.nostorage.NoIntDomainStorage

/**
  * Model for Linear Programming
  * @param solverLib the solver to use
  * @param p: the model from which to inherit
  */
class LPModel[I <: MPSolverInterface](p: UninstantiatedModel, solverLib: MPSolverLib[I]) extends InstantiatedModel(p){
  implicit lazy val solver: MPSolver[I] = new MPSolver(solverLib.createSolver)

  override type IntVarImplementation = NoIntDomainStorage
  override type FloatVarImplementation = MIPFloatVar[I]

  override protected def instantiateIntVar(content: Iterable[Int], name: String): NoIntDomainStorage = throw new RuntimeException("There is no support for Integer variables in linear programming")

  override protected def instantiateFloatVar(min: Double, max: Double, name: String): MIPFloatVar[I] = MIPFloatVar(min, max, name, solver)

  override def post(constraint: Constraint): Unit = constraint match {
    case ExpressionConstraint(expr) => postBooleanExpr(expr)
    case _ => throw new RuntimeException("No support for constraint "+constraint.getClass.getName+" in linear programming")
  }

  private def postBooleanExpr(expr: BoolExpression): Unit = {
    expr match {
      case EqFloat(array) =>
        val expressions = array.map(getLinearExpression)
        for(i <- 0 until expressions.length)
          for(j <- i+1 until expressions.length)
            oscar.linprog.modeling.add(expressions(i) =:= expressions(j))
      case GrEqFloat(a,b) =>
        val exprA = getLinearExpression(a)
        val exprB = getLinearExpression(b)
        oscar.linprog.modeling.add(exprA >:= exprB)
      case LrEqFloat(a,b) =>
        val exprA = getLinearExpression(a)
        val exprB = getLinearExpression(b)
        oscar.linprog.modeling.add(exprA <:= exprB)
      case _ => throw new RuntimeException("No support for expression "+expr.getClass.getName+" in linear programming")
    }
  }

  private def getLinearExpression(expr: FloatExpression): LinearExpression = expr match {
    case Constant(c) => c
    case Minus(a, b) => getLinearExpression(a) - getLinearExpression(b)
    case Sum(array) =>
      val exprs = array.map(getLinearExpression)
      var s = exprs(0)
      for(i <- 1 until exprs.length)
        s = s + exprs(i)
      s
    case UnaryMinus(a) => -getLinearExpression(a)
    case Prod(array) =>
      var mainExpr: FloatExpression = null
      var multiplier = 1.0
      for(expr2 <- array) expr2 match {
        case Constant(b) => multiplier *= b
        case bound if bound.isBound => multiplier *= bound.evaluate()
        case other =>
          if(mainExpr == null)
            mainExpr = other
          else
            throw new RuntimeException("Only product with constants are allowed")
      }
      if(mainExpr == null)
        multiplier
      else
        multiplier * getLinearExpression(mainExpr)
    case v: FloatVar => floatRepresentatives.get(v).realMipVar
    case _ => throw new RuntimeException("No support for expression "+expr.getClass.getName+" in linear programming")
  }

  override protected def postObjective(optimisationMethod: OptimisationMethod): Unit = optimisationMethod match {
    case MinimisationFloat(o) => solver.setObjective(getLinearExpression(o), min=true)
    case MaximisationFloat(o) => solver.setObjective(getLinearExpression(o), min=false)
    case NoOptimisation() => //nothing
    case _ => throw new RuntimeException("You can only minimize/maximize float expression in linear programming")
  }
}

object LPModel {
  def lpsolve(p: UninstantiatedModel) = new LPModel(p, LPSolveLib)
  def gurobi(p: UninstantiatedModel) = new LPModel(p, GurobiLib)
}