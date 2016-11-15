package oscar.cp.constraints

import oscar.algo.search.Outcome
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.algo.search.Outcome._
import oscar.cp.core.CPStore

final class EqualityBC(x: CPIntVar, y: CPIntVar) extends Constraint(x.store, "EqualityBC") {

  priorityL2 = CPStore.MaxPriorityL2
  idempotent = true
  
  final override def setup(l: CPPropagStrength): Outcome = {
    val outcome = propagate()
    if (outcome != Suspend) outcome
    else {
      x.callPropagateWhenBoundsChange(this)
      y.callPropagateWhenBoundsChange(this)
      Suspend
    }
  }

  final override def propagate(): Outcome = {
    var yMin = y.min
    var yMax = y.max
    if (yMin == yMax) assignTo(x, yMin)
    else {
      var xMin = x.min
      var xMax = x.max
      if (xMin == xMax) assignTo(y, xMin)
      else {
        // Update min
        while (yMin != xMin) {
          if (y.updateMin(xMin) == Failure) return Failure
          yMin = y.min
          if (yMin != xMin) {
            if (x.updateMin(yMin) == Failure) return Failure
            xMin = x.min
          }
        }
        // Update max
        while (yMax != xMax) {
          if (y.updateMax(xMax) == Failure) return Failure
          yMax = y.max
          if (yMax != xMax) {
            if (x.updateMax(yMax) == Failure) return Failure
            xMax = x.max
          }
        }
        Suspend
      }
    }
  }

  @inline private def assignTo(intVar: CPIntVar, value: Int): Outcome = {
    if (intVar.assign(value) == Failure) Failure
    else Success
  }
}