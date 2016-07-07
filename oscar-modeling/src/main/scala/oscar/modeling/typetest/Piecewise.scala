package oscar.modeling.typetest

case class Piecewise[T <: Number, U <: ValueType](param: Expression[T], cutoffs: Array[T], pieces: Array[Expression[U]]) extends Expression[U] {
  def evaluate(data: Data): U = {
    val position = param.evaluate(data).asDouble
    for (i <- cutoffs.indices)
      if (position < cutoffs(i).asDouble)
        return pieces(i).evaluate(data)
    pieces.last.evaluate(data)
  }

  def subExpressions() = pieces
}
