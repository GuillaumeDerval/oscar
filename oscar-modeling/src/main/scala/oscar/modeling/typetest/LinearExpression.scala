package oscar.modeling.typetest

class LinearExpression(val vars: Array[DoubleVar], val coeffs: Array[ConstantLike[Number]])
  extends WeightedSum[Number](vars.asInstanceOf[Array[Expression[Number]]], coeffs)