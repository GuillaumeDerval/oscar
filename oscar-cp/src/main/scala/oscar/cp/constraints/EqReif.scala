package oscar.cp.constraints

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore

/** @author Renaud Hartert ren.hartert@gmail.com */
final class EqReif(int: CPIntVar, value: Int, boolean: CPBoolVar) extends Constraint(int.store, "EqReif") {

  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2

  final override def setup(l: CPPropagStrength): Unit = {
    propagate()
    if(isActive) {
      int.callPropagateWhenDomainChanges(this)
      boolean.callPropagateWhenBind(this)
    }
  }

  final override def propagate(): Unit = {
    if (boolean.isTrue) {
      int.assign(value)
      deactivate()
    } else if (boolean.isFalse) {
      int.removeValue(value)
      deactivate()
    } else if (!int.hasValue(value)) {
      boolean.assignFalse()
      deactivate()
    } else if (int.isBound) {
      boolean.assignTrue()
      deactivate()
    }
  }
}