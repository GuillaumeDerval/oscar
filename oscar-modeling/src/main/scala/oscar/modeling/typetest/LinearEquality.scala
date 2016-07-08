package oscar.modeling.typetest

import oscar.modeling.constraints.Constraint

case class LinearEquality(expr: LinearExpression, value: ConstantLike[Number]) extends Constraint