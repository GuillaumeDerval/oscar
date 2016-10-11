package constraints

import algebra.IntExpression

case class MinAssignment(xarg: Array[IntExpression], weightsarg: Array[Array[Int]], cost: IntExpression) extends Constraint {}