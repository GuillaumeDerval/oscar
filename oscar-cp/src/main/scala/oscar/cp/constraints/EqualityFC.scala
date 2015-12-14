package oscar.cp.constraints

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

class EqualityFC(x: CPIntVar, y: CPIntVar) extends Constraint(x.store, "EqualityFC") {

  override def setup(l: CPPropagStrength): CPOutcome = {
    if (propagate() == Failure) Failure
    else {
      x.callPropagateWhenBind(this)
      y.callPropagateWhenBind(this)
      Suspend
    }
  }

  final override def propagate(): CPOutcome = {
    if (x.isBound) if (y.assign(x.min) == Failure) Failure else Success
    else if (x.assign(y.min) == Failure) Failure else Success
  }
}