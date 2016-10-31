package oscar.linprog

import oscar.algebra._

class MPModel(val interface: SolverInterface[Linear,Linear,Double]) extends Model[Linear,Linear,Double]{

  implicit val thisModel = this

  type LinearConstraint = Equation[Linear, Double]

  @deprecated("to be replaced by VarNumerical")
  def MPFloatVar(name: String, lb: Double = Double.MinValue, ub: Double = Double.MaxValue): VarNumerical = VarNumerical(name, lb, ub)

  @deprecated("to be replaced by VarInt")
  def MPIntVar(name: String, rng: Range): VarInt = VarInt(name, rng)

  @deprecated("to be replaced by VarBinary")
  def MPBinaryVar(name: String): VarBinary = VarBinary(name)

  def add(eq: EquationDescription[Linear, Double], name: String = ""): Unit = {
    subjectTo(name |: eq)
  }

  def add(eq: Equation[Linear, Double]): Unit = {
    subjectTo(eq)
  }

  def maximize(obj: NormalizedExpression[Linear,Double]): Unit = {
    withObjective(Maximize(obj))
  }

  def minimize(obj: NormalizedExpression[Linear,Double]): Unit = {
    withObjective(Minimize(obj))
  }

  def solve: ModelStatus[Linear,Linear,Double] = interface.solve(this)
}

object Migration {

  implicit def int2DoubleConst(i: Int): NormalizedExpression[Constant, Double] = Const(i.toDouble).normalized

  implicit class ExpressionWithColon(val expr: Expression[Linear, Double]) extends AnyVal {
    @deprecated("to be replaced by <=")
    def <:=(that: NormalizedExpression[Linear, Double]) = expr <= that

    @deprecated("to be replaced by >=")
    def >:=(that: NormalizedExpression[Linear, Double]) = expr >= that

    @deprecated("to be replaced by ===")
    def =:=(that: NormalizedExpression[Linear, Double]) = expr === that
  }

}
