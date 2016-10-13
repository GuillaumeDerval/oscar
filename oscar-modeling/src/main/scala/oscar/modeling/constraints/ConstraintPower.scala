package oscar.modeling.constraints

/**
 * Some flags that can be used on constraint. Flags really used depends on the method you use to solve the model
 */
object ConstraintPower extends Enumeration {
  type ConstraintPower = Value

  //CP
  val Weak, Medium, Strong, Automatic = ConstraintPower
}
