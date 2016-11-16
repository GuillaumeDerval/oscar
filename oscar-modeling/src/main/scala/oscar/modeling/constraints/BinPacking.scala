package oscar.modeling.constraints

import oscar.modeling.algebra.integer.IntExpression

case class BinPacking(x: Array[IntExpression], w: Array[Int], l: Array[IntExpression]) extends Constraint {}