package oscar.modeling.typetest

import oscar.modeling.constraints.Constraint

sealed trait LinearInequality extends Constraint

case class LinearLeq(expr: LinearExpression, value: ConstantLike[Number]) extends LinearInequality
case class LinearGeq(expr: LinearExpression, value: ConstantLike[Number]) extends LinearInequality