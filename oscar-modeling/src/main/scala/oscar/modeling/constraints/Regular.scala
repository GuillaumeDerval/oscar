package oscar.modeling.constraints

import oscar.cp.constraints.Automaton
import oscar.modeling.algebra.integer.IntExpression

/**
 * Impose that the values of the variables in 'on' can be emitted by a particular automaton
 */
case class Regular(on: Array[IntExpression], automaton: Automaton) extends Constraint {}