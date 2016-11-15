package oscar.cp.constraints

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.algo.search.Outcome._
import oscar.algo.search.Outcome

final class DiffVal(x: CPIntVar, v: Int) extends Constraint(x.store, "DiffVal") {
  
  final override def setup(l: CPPropagStrength): Outcome = {
    if (x.removeValue(v) == Failure) Failure
    else Success
  }
}