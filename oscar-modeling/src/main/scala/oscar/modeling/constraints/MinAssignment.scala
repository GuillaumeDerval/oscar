package oscar.modeling.constraints

import oscar.modeling.algebra.IntExpression

case class MinAssignment(xarg: Array[IntExpression], weightsarg: Array[Array[Int]], cost: IntExpression) extends Constraint {}