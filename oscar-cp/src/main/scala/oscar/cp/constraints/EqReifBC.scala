package oscar.cp.constraints

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

/** @author Renaud Hartert ren.hartert@gmail.com */
final class EqReifBC(int: CPIntVar, value: Int, boolean: CPBoolVar) extends Constraint(int.store, "EqReif") {

  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2

  final override def setup(l: CPPropagStrength): CPOutcome = {
    val outcome = propagate()
    if (outcome != Suspend) outcome
    else {
      int.callPropagateWhenBoundsChange(this)
      boolean.callPropagateWhenBind(this)
      Suspend
    }
  }

  final override def propagate(): CPOutcome = {
    if (boolean.isFalse) {
      if (int.min == value) {
        if (int.updateMin(value + 1) == Failure) Failure
        else Success
      } else if (int.max == value) {
        if (int.updateMax(value - 1) == Failure) Failure
        else Success
      } else Suspend
    } else if (boolean.isTrue) {
      if (int.assign(value) == Failure) Failure
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