package oscar.cp.constraints

import oscar.algo.search.Outcome
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.algo.search.Outcome._

/** @author Renaud Hartert ren.hartert@gmail.com */
class BinaryClause(x: CPBoolVar, y: CPBoolVar, name: String) extends Constraint(x.store, name) {

  final override def setup(l: CPPropagStrength): Outcome = {
    val outcome = propagate()
    if (outcome != Suspend) outcome
    else {
      x.callPropagateWhenBind(this)
      y.callPropagateWhenBind(this)
      Suspend
    }
  }

  final override def propagate(): Outcome = {
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