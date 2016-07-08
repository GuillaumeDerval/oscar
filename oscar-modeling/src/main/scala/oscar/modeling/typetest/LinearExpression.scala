package oscar.modeling.typetest

class LinearExpression(vars: Array[Var[Number]], coeffs: Array[ConstantLike[Number]])
  extends WeightedSum[Number](vars.asInstanceOf[Array[Expression[Number]]], coeffs)