package oscar.modeling.models.lp

import oscar.algebra.{LinearExpression, Solution}
import oscar.linprog.MPModel
import oscar.linprog.lpsolve.LPSolve
import oscar.modeling.algebra.bool.{BoolExpression, EqFloat, GrEqFloat, LrEqFloat}
import oscar.modeling.algebra.floating._
import oscar.modeling.constraints.{Constraint, ExpressionConstraint}
import oscar.modeling.models._
import oscar.modeling.vars.FloatVar
import oscar.modeling.vars.mip.MIPFloatVar
import oscar.modeling.vars.mip.MIPFloatVar.MIPSolutionHolder
import oscar.modeling.vars.nostorage.NoIntDomainStorage

/**
  * Model for Linear Programming
  * @param p: the model from which to inherit
  */
class LPModel(p: UninstantiatedModel) extends InstantiatedModel(p) with MIPSolutionHolder {
  implicit lazy val oscarLPModel = new MPModel(LPSolve)
  private var currentEqId = 0

  override type IntVarImplementation = NoIntDomainStorage
  override type FloatVarImplementation = MIPFloatVar

  override protected def instantiateIntVar(content: Iterable[Int], name: String): NoIntDomainStorage = throw new RuntimeException("There is no support for Integer variables in linear programming")

  override protected def instantiateFloatVar(min: Double, max: Double, name: String): MIPFloatVar = MIPFloatVar(min, max, name, oscarLPModel, this)

  override def post(constraint: Constraint): Unit = constraint match {
    case ExpressionConstraint(expr) => postBooleanExpr(expr)
    case _ => throw new RuntimeException("No support for constraint "+constraint.getClass.getName+" in linear programming")
  }

  private def postBooleanExpr(expr: BoolExpression): Unit = {
    expr match {
      case EqFloat(array) =>
        val expressions = array.map(getLinearExpression)
        for(i <- 0 until expressions.length)
          for(j <- i+1 until expressions.length) {
            oscarLPModel.subjectTo((expressions(i) === expressions(j)).|:(currentEqId.toString))
            currentEqId+=1
          }
      case GrEqFloat(a,b) =>
        val exprA = getLinearExpression(a)
        val exprB = getLinearExpression(b)
        oscarLPModel.subjectTo((exprA >= exprB).|:(currentEqId.toString))
        currentEqId+=1
      case LrEqFloat(a,b) =>
        val exprA = getLinearExpression(a)
        val exprB = getLinearExpression(b)
        oscarLPModel.subjectTo((exprA <= exprB).|:(currentEqId.toString))
        currentEqId+=1
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
    case MinimisationFloat(o) => oscarLPModel.minimize(getLinearExpression(o))
    case MaximisationFloat(o) => oscarLPModel.maximize(getLinearExpression(o))
    case NoOptimisation() => //nothing
    case _ => throw new RuntimeException("You can only minimize/maximize float expression in linear programming")
  }

  protected var currentSolution: Option[Solution[Double]] = None
  def setCurrentSolution(c: Option[Solution[Double]]) = currentSolution = c
  override def getSolution: Option[Solution[Double]] = currentSolution
}