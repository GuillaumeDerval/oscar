package oscar.modeling.typetest

import scala.collection.immutable.HashMap
import scala.collection.mutable

// To be adapted to Guillaume's oscar.modeling.models.Model definition

// Caveat: for simplicity, we currently assume all variables are non-negative

trait LinearModel // extends Model

object LinearModel {
  def apply(x: Array[DoubleVar], obj: Array[ConstantLike[Number]],
            vars: Array[Array[DoubleVar]], coeffs: Array[Array[ConstantLike[Number]]],
            values: Array[ConstantLike[Number]]) = new ParametrizedLinearModel(x, obj, vars, coeffs, values)

  def apply(constraints: Array[LinearEquality], obj: LinearExpression) = {
    var varSet = new mutable.HashSet[DoubleVar]()
    val nRows = constraints.length
    val vars = new Array[Array[DoubleVar]](nRows)
    val coeffs = new Array[Array[ConstantLike[Number]]](nRows)
    val values = new Array[ConstantLike[Number]](nRows)

    for (i <- 0 until nRows) {
      vars(i) = constraints(i).expr.vars
      coeffs(i) = constraints(i).expr.coeffs
      values(i) = constraints(i).value
    }

    vars.flatten.foreach(varSet += _)
    obj.vars.foreach(varSet += _)
    val x = varSet.toArray
    val indexOf = Map(x.zipWithIndex:_*)
    val objTab: Array[ConstantLike[Number]] = Array.fill(x.length)(Constant(0))
    for ((v,c) <- obj.vars.zip(obj.coeffs)) objTab(indexOf(v)) = c

    new ParametrizedLinearModel(x, objTab, vars, coeffs, values)
  }

  def apply(constraints: Array[LinearEquality], inequalities: Array[LinearInequality], obj: LinearExpression) = {
    // TODO: change the "z" name to something unique
    val surplusConstraints = inequalities.map {
      case LinearLeq(expr, value) => LinearEquality(new LinearExpression(expr.vars :+ new DoubleVar("z", 0, Double.PositiveInfinity),
        expr.coeffs :+ Constant(1)), value)
      case LinearGeq(expr, value) => LinearEquality(new LinearExpression(expr.vars :+ new DoubleVar("z", 0, Double.PositiveInfinity),
        expr.coeffs :+ Constant(-1)), value)
    }
    LinearModel(constraints ++ surplusConstraints, obj)
  }
}

/**
  * Represents the linear program:
  *   min c*x, subject to A*x = b, x >= 0
  *
  * @param x Vector x
  * @param obj Vector c
  * @param vars For each row of A the variables involved
  * @param coeffs For each row of A the corresponding coefficients
  * @param values Vector b
  */
class ParametrizedLinearModel private (val x: Array[DoubleVar], val obj: Array[ConstantLike[Number]],
                                       val vars: Array[Array[DoubleVar]], val coeffs: Array[Array[ConstantLike[Number]]],
                                       val values: Array[ConstantLike[Number]]) extends LinearModel

class FixedLinearModel(x: Array[Var[Number]], obj: Array[Double],
                       vars: Array[Array[DoubleVar]], coeffs: Array[Array[Double]],
                       values: Array[Double]) extends LinearModel