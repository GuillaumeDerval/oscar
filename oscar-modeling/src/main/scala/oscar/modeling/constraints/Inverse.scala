package oscar.modeling.constraints

import oscar.modeling.algebra.integer.IntExpression

case class Inverse(prev: Array[IntExpression], next: Array[IntExpression]) extends Constraint {}
