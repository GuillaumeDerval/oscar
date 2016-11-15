package oscar.cp.constraints

import oscar.algo.search.Outcome
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.algo.search.Outcome._

class EqualityFC(x: CPIntVar, y: CPIntVar) extends Constraint(x.store, "EqualityFC") {

  override def setup(l: CPPropagStrength): Outcome = {
    if (propagate() == Failure) Failure
    else {
      x.callPropagateWhenBind(this)
      y.callPropagateWhenBind(this)
      Suspend
    }
  }

  final override def propagate(): Outcome = {
    if (x.isBound) if (y.assign(x.min) == Failure) Failure else Success
    else if (x.assign(y.min) == Failure) Failure else Success
  }
}