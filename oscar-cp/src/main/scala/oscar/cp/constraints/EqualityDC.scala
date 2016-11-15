package oscar.cp.constraints

import oscar.algo.search.Outcome
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.algo.search.Outcome._
import oscar.cp.core.CPStore

final class EqualityDC(x: CPIntVar, y: CPIntVar) extends Constraint(x.store, "EqualityDC") {

  private[this] val values = new Array[Int](Math.max(x.size, y.size))

  override def setup(l: CPPropagStrength): Outcome = {
    val outcome = init()
    if (outcome != Suspend) outcome
    else {
      val propagatorX = x.callOnChanges(s => removeValues(x, y, s))
      propagatorX.priority = CPStore.MaxPriorityL2
      val propagatorY = y.callOnChanges(s => removeValues(y, x, s))
      propagatorY.priority = CPStore.MaxPriorityL2
      Suspend
    }
  }

  @inline private def init(): Outcome = {
    if (x.isBound) {
      if (y.assign(x.min) == Failure) Failure 
      else Success
    }
    else if (y.isBound) {
      if (x.assign(y.min) == Failure) Failure 
      else Success
    }
    else {
      var i = x.fillArray(values)
      while (i > 0) {
        i -= 1
        val value = values(i)
        if (!y.hasValue(value) && x.removeValue(value) == Failure) return Failure
      }
      i = y.fillArray(values)
      while (i > 0) {
        i -= 1
        val value = values(i)
        if (!x.hasValue(value) && y.removeValue(value) == Failure) return Failure
      }
      Suspend
    }
  }

  @inline private def removeValues(from: CPIntVar, to: CPIntVar, delta: DeltaIntVar): Outcome = {
    if (from.isBound) {
      if (to.assign(from.min) == Failure) Failure
      else Success
    } else {
      val nValues = delta.fillArray(values)
      to.removeValues(values, nValues)
    }
  }
}