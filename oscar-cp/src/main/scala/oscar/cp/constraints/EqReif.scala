package oscar.cp.constraints

import oscar.algo.search.Outcome
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.algo.search.Outcome._

/** @author Renaud Hartert ren.hartert@gmail.com */
final class EqReif(int: CPIntVar, value: Int, boolean: CPBoolVar) extends Constraint(int.store, "EqReif") {

  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2

  final override def setup(l: CPPropagStrength): Outcome = {
    val outcome = propagate()
    if (outcome != Suspend) outcome
    else {
      int.callPropagateWhenDomainChanges(this)
      boolean.callPropagateWhenBind(this)
      Suspend
    }
  }

  final override def propagate(): Outcome = {
    if (boolean.isTrue) {
      if (int.assign(value) == Failure) Failure
      else Success
    } else if (boolean.isFalse) {
      if (int.removeValue(value) == Failure) Failure
      else Success
    } else if (!int.hasValue(value)) {
      if (boolean.assignFalse() == Failure) Failure
      else Success
    } else if (int.isBound) {
      if (boolean.assignTrue() == Failure) Failure
      else Success
    } else Suspend
  }
}