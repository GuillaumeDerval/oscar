package oscar.cp.constraints

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

final class DiffVar(x: CPIntVar, y: CPIntVar) extends Constraint(x.store, "DiffVar") {
  
  idempotent = true

  final override def setup(l: CPPropagStrength): CPOutcome = {
    val outcome = init()
    if (outcome != Suspend) outcome
    else {
      x.callPropagateWhenBind(this)
      y.callPropagateWhenBind(this)
      Suspend
    }
  }
  
  @inline private def init(): CPOutcome = {
    if (x.isBound) {
      if (y.removeValue(x.min) == Failure) Failure
      else Success
    } else if (y.isBound) {
      if (x.removeValue(y.min) == Failure) Failure
      else Success
    } else Suspend
  }

  @inline final override def propagate(): CPOutcome = {
    if (x.isBound) {
      if (y.removeValue(x.min) == Failure) Failure
      else Success
    } else if (x.removeValue(y.min) == Failure) Failure
    else Success
  }
}