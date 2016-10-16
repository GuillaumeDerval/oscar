package oscar.modeling.constraints

import oscar.modeling.algebra.IntExpression

/**
  * Ensures at least two values in x are different (they are not all equal)
  */
case class NotAllEqual(x: Array[IntExpression]) extends Constraint {}
