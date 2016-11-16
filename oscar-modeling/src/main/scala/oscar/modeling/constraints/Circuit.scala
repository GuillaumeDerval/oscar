package oscar.modeling.constraints

import oscar.modeling.algebra.integer.IntExpression

case class Circuit(succ: Array[IntExpression], symmetric: Boolean = true) extends Constraint {}
case class SubCircuit(succ: Array[IntExpression], offset: Int = 0) extends Constraint {}