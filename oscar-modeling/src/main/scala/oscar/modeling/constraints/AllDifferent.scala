package oscar.modeling.constraints

import oscar.modeling.algebra.integer.IntExpression

/**
 * Imposes that a given BoolExpression is true
 *
 * @param array
 */
case class AllDifferent(array: Array[IntExpression]) extends Constraint {}