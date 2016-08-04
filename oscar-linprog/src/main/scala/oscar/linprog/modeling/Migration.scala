package oscar.linprog.modeling

import oscar.algebra.{AbstractExpression, Const, Equation, EquationDescription, Expression,  Linear, Model, Var}
import oscar.linprog.interface.MPSolverInterface

/**
  * Created by smo on 27/07/16.
  */
object Migration {

  implicit def int2DoubleConst(i: Int) = Const(i.toDouble).toExpression

  implicit class ExpressionWithColon(val expr: AbstractExpression[Linear, Double]) extends AnyVal {
    def <:=(that: Expression[Linear, Double]) = expr <= that

    def >:=(that: Expression[Linear, Double]) = expr >= that

    def =:=(that: Expression[Linear, Double]) = expr === that
  }

  def add[I <: MPSolverInterface](eq: EquationDescription[Linear, Double], name: String = "")(implicit solver: MPSolver[I]) = {
    LinearConstraint(name ||: eq)
  }

  def add[I <: MPSolverInterface](eq: Equation[Linear, Double])(implicit solver: MPSolver[I]) = {
    LinearConstraint(eq)
  }

}

case class VarBinary(name: String)(implicit model: Model[_, _,Double]) extends Var[Double] {
  val id = model.addVariable((this))
  def lowerBound = 0

  def upperBound = 1

  def value = None
}


case class VarInt(name: String, lowerBound: Double , upperBound: Double )(implicit model: Model[_, _,Double]) extends Var[Double] {
  val id = model.addVariable((this))
  def value = None
}

case class VarNumerical(name: String, lowerBound: Double , upperBound: Double )(implicit model: Model[_, _,Double]) extends Var[Double]{
  val id = model.addVariable((this))
  def value = None
}