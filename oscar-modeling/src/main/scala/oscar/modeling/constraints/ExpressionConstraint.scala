package oscar.modeling.constraints

import oscar.modeling.algebra.BoolExpression

/**
 * Imposes that a given BoolExpression is true
 * @param expr
 */
case class ExpressionConstraint(expr: BoolExpression) extends Constraint {}
