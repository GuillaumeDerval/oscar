package oscar.cp.nogoods.core

import oscar.algo.search.Outcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint

/** @author Renaud Hartert ren.hartert@gmail.com */
class Unfeasible(store: CPStore) extends Constraint(store, "Unfeasible") {
  
  override def setup(l: CPPropagStrength): Outcome = Outcome.Failure

}