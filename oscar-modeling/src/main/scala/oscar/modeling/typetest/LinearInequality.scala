package oscar.modeling.typetest

import oscar.modeling.constraints.Constraint

sealed trait LinearInequality extends Constraint

case class LinearGt(expr: LinearExpression, value: ConstantLike[Number]) extends LinearInequality
case class LinearGtZero(expr: LinearExpression) extends LinearInequality

case class LinearLt(expr: LinearExpression, value: ConstantLike[Number]) extends LinearInequality
case class LinearLtZero(expr: LinearExpression) extends LinearInequality