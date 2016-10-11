package constraints

import algebra.IntExpression

case class Inverse(prev: Array[IntExpression], next: Array[IntExpression]) extends Constraint {}
