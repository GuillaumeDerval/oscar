package constraints

import algebra.IntExpression

/**
 * Imposes that a given BoolExpression is true
 * @param array
 */
case class AllDifferent(array: Array[IntExpression]) extends Constraint {}
