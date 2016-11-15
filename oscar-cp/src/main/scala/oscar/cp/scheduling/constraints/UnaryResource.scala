package oscar.cp.scheduling.constraints

import oscar.algo.search.Outcome
import oscar.algo.search.Outcome._
import oscar.cp.core.CPPropagStrength._
import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.cp.core.variables.CPIntVar

class UnaryResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int = 1)
  extends Constraint(starts.head.store, "UnaryResource") {
  override def setup(l: CPPropagStrength): Outcome = {

    val unitDemand = CPIntVar(1)(s)
    val demands = Array.fill(starts.size)(unitDemand)

    l match {
      case Weak =>
        if (s.post(TTPerTask(starts, durations, ends, demands, resources, unitDemand, id)) == Failure) return Failure
      // steven, stop adding things here, weak = time-table only
      case Automatic =>
        if (s.post(TTPerTask(starts, durations, ends, demands, resources, unitDemand, id)) == Failure) return Failure
        if (s.post(Unary(starts, durations, ends, resources, id)) == Failure) return Failure
      case Medium =>
        if (s.post(TTPerTask(starts, durations, ends, demands, resources, unitDemand, id)) == Failure) return Failure
        if (s.post(Unary(starts, durations, ends, resources, id)) == Failure) return Failure
      case Strong =>
        if (s.post(TTPerTask(starts, durations, ends, demands, resources, unitDemand, id)) == Failure) return Failure
        if (s.post(Unary(starts, durations, ends, resources, id)) == Failure) return Failure
    }

    Success
  }
}


