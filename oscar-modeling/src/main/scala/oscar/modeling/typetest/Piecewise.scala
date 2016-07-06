package oscar.modeling.typetest

class Piecewise[T <: Number, U <: ValueType](param: Expression[T], cutoffs: Array[T], pieces: Array[Expression[U]]) extends Expression[U] {
  def evaluate(): U = {
    val position = param.evaluate().asDouble
    for (i <- cutoffs.indices)
      if (position < cutoffs(i).asDouble)
        return pieces(i).evaluate()
    pieces.last.evaluate()
  }

  def subExpressions() = pieces
}
