package constraints

import algebra.IntExpression

case class BinPacking(x: Array[IntExpression], w: Array[Int], l: Array[IntExpression]) extends Constraint {}