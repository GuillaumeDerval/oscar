package constraints

import algebra.BoolExpression

/**
 * Imposes that a given BoolExpression is true
 * @param expr
 */
case class ExpressionConstraint(expr: BoolExpression) extends Constraint {}
