package oscar.linprog

import oscar.algebra._

class MPModel[S](val interface: SolverInterface[S,Linear,Linear,Double]) extends Model[Linear,Linear,Double]{

  implicit val thisModel = this

  type LinearConstraint[V] = Equation[Linear,Double]

  def MPFloatVar(name: String, lb: Double = Double.MinValue, ub: Double = Double.MaxValue) = VarNumerical(name,lb,ub)
  def MPIntVar(name: String, rng: Range) = VarInt(name,rng.min,rng.max)
  def MPBinaryVar(name: String) = VarBinary(name)
  def add(eq: EquationDescription[Linear, Double], name: String = "") = {
    subjectTo(name |: eq)
  }

  def maximize(obj: NormalizedExpression[Linear,Double]): Unit = {
    withObjective(Maximize(obj))
  }

  def minimize(obj: NormalizedExpression[Linear,Double]): Unit = {
    withObjective(Minimize(obj))
  }

  def solve = interface.solve(this)

  def add(eq: Equation[Linear, Double]) = {
    subjectTo(eq)
  }

}

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

}

/**
  * Created by smo on 12/10/16.
  */
class Migration {

}
