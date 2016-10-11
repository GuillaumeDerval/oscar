package constraints

import algebra.IntExpression

case class Circuit(succ: Array[IntExpression], symmetric: Boolean) extends Constraint {}
case class SubCircuit(succ: Array[IntExpression], offset: Int = 0) extends Constraint {}