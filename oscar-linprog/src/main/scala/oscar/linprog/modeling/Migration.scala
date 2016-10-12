package oscar.linprog.modeling

import oscar.algebra.{Expression, Const, Equation, EquationDescription, NormalizedExpression,  Linear, Model, Var}
import oscar.linprog.interface.MPSolverInterface

/**
  * Created by smo on 27/07/16.
  */
object Migration {

  implicit def int2DoubleConst(i: Int) = Const(i.toDouble).normalized

  implicit class ExpressionWithColon(val expr: Expression[Linear, Double]) extends AnyVal {
    def <:=(that: NormalizedExpression[Linear, Double]) = expr <= that

    def >:=(that: NormalizedExpression[Linear, Double]) = expr >= that

    def =:=(that: NormalizedExpression[Linear, Double]) = expr === that
  }

  def add[I <: MPSolverInterface](eq: EquationDescription[Linear, Double], name: String = "")(implicit solver: MPSolver[I]) = {
    LinearConstraint(name ||: eq)
  }

  def add[I <: MPSolverInterface](eq: Equation[Linear, Double])(implicit solver: MPSolver[I]) = {
    LinearConstraint(eq)
  }

}
