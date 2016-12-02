package oscar.cp.constraints

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint

final class DiffVal(x: CPIntVar, v: Int) extends Constraint(x.store, "DiffVal") {
  override def setup(l: CPPropagStrength): Unit = {
    x.removeValue(v)
  }
}