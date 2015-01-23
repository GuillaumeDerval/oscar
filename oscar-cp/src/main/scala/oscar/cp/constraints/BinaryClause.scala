package oscar.cp.constraints

import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

/** @author Renaud Hartert ren.hartert@gmail.com */
class BinaryClause(x: CPBoolVar, y: CPBoolVar, name: String) extends Constraint(x.store, name) {

  final override def setup(l: CPPropagStrength): CPOutcome = {
    val outcome = propagate()
    if (outcome == Failure) Failure
    else if (outcome == Success) Success
    else {
      x.callPropagateWhenBind(this)
      y.callPropagateWhenBind(this)
      Suspend
    }
  }

  final override def propagate(): CPOutcome = {
    if (x.isTrue) Success
    else if (y.isTrue) Success
    else if (x.isFalse) {
      if (y.assign(1) == Failure) Failure
      else Success
    } else if (y.isFalse) { 
      if (x.assign(1) == Failure) Failure
      else Success
    }
    else Suspend
  }
}