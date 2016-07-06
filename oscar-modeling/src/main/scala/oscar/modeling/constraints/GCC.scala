package oscar.modeling.constraints

import oscar.modeling.algebra.IntExpression

case class GCC(x: Array[IntExpression], minval: Int, low: Array[Int], up: Array[Int]) extends Constraint {}

object GCC {
  /**
    * Global Cardinality Constraint: every value occurs at least min and at most max
    * @param x an non empty array of variables
    * @param values is the range of constrained values
    * @param min is the minimum number of occurrences for each value in the range values
    * @param max is the maximum number of occurrences for each value in the range values
    * @return a constraint such that each value in the range values occurs at least min and at most max times.
    */
  def apply(x: Array[IntExpression], values: Range, min: Int, max: Int): Constraint = {
    new GCC(x, values.min, Array.fill(values.size)(min), Array.fill(values.size)(max))
  }

  /**
    * Global Cardinality Constraint: every value v occurs at least min(v) and at most max(v)
    * @param x an non empty array of variables
    * @param values is the range of constrained values
    * @param min is the minimum number of occurrences for each value in the range values
    * @param max is the maximum number of occurrences for each value in the range values
    * @return a constraint such that each value in the range values occurs at least min and at most max times.
    */
  def apply(x: Array[IntExpression], values: Range, min: Array[Int], max: Array[Int]): Constraint = {
    new GCC(x, values.min, min, max)
  }
}