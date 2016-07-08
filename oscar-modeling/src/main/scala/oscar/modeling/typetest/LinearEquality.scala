package oscar.modeling.typetest

import oscar.modeling.constraints.Constraint

class LinearEquality(val vars: Array[Var[Number]], val coeffs: Array[ConstantLike[Number]], val value: ConstantLike[Number]) extends Constraint