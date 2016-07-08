package oscar.modeling.typetest

// To be adapted to Guillaume's oscar.modeling.models.Model definition

trait LinearProgram //extends Model

/**
  * Represents the linear program:
  *   min c*x, subject to A*x = b, x >= 0
  * @param vars For each row of A the variables involved
  * @param coeffs For each row of A the corresponding coefficients
  * @param values Vector b
  * @param obj Vector c
  */
class ParametrizedLinearProgram(vars: Array[Array[Var[Number]]], coeffs: Array[Array[ConstantLike[Number]]],
                                values: Array[ConstantLike[Number]], obj: Array[ConstantLike[Number]]) extends LinearProgram

object ParametrizedLinearProgram {
  def apply(constraints: Array[LinearEquality], obj: Array[ConstantLike[Number]]) = {
    val nRows = constraints.length
    val vars = new Array[Array[Var[Number]]](nRows)
    val coeffs = new Array[Array[ConstantLike[Number]]](nRows)
    val values = new Array[ConstantLike[Number]](nRows)

    for (i <- 0 until nRows) {
      vars(i) = constraints(i).vars
      coeffs(i) = constraints(i).coeffs
      values(i) = constraints(i).value
    }

    new ParametrizedLinearProgram(vars, coeffs, values, obj)
  }

  def apply(constraints: Array[LinearEquality], inequalities: Array[LinearInequality], obj: Array[ConstantLike[Number]]): LinearProgram = ???
}

class FixedLinearProgram(vars: Array[Array[DoubleVar]], coeffs: Array[Array[Double]],
                         values: Array[Double], obj: Array[Double]) extends LinearProgram