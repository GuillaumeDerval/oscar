package oscar.modeling.solvers.cp.branchings

import oscar.modeling.models.CPModel
import oscar.modeling.solvers.cp.branchings.Branching.Alternative
import oscar.modeling.vars.IntVar

object ConflictOrderingSearch {
  def apply(cp: CPModel, variables: Array[IntVar], varHeuristic: (Int) => Int, valHeuristic: (Int) => Int, doReset: Boolean = false) =
    new ConflictOrderingSearch(cp, variables, varHeuristic, valHeuristic, doReset)
}

class ConflictOrderingSearch(cp: CPModel, variables: Array[IntVar], varHeuristic: (Int) => Int, valHeuristic: (Int) => Int, doReset: Boolean = false) extends Branching {

  require(variables.length > 0)

  var lastVariables = List[Int]()

  var lastVariable: Option[Int] = None
  var lastDepth = 0
  val depth = cp.getReversibleInt(0)

  override def reset() = {
    lastVariable = None
    lastDepth = 0
    if (doReset) lastVariables = List[Int]()
  }

  override def alternatives(): Seq[Alternative] = {
    val d = depth.incr

    // Step 1: if last conflicting variable is new, add in head position.
    if (d <= lastDepth) lastVariable foreach { x =>
      // move x to head if it is in lastVariables
      lastVariables = lastVariables filter (_ != x)
      lastVariables = x +: lastVariables

      lastVariable = None
    }

    lastDepth = d

    // Step 2: if some variable in conflict set is not bound, branch on it
    lastVariables.foreach { i =>
      val x = variables(i)
      if (!x.isBound) {
        lastVariable = Some(i)
        val value = valHeuristic(i)
        val alternatives = Branching.branch { cp.post(x === value) } { cp.post(x !== value) }
        return alternatives
      }
    }

    // Step 3: if all conflict set variables are bound, ask heuristic
    var bestVar = -1
    var bestScore = Int.MaxValue
    var p = variables.length
    while (p > 0) {
      p -= 1
      if (!variables(p).isBound) {
        val score = varHeuristic(p)
        if (score <= bestScore) {
          bestVar = p
          bestScore = score
        }
      }
    }


    if (bestVar == -1) {  // all variables where bound, solution!
      lastVariable = None
      Branching.noAlternative
    }
    else {
      lastVariable = Some(bestVar)
      val x = variables(bestVar)
      val value = valHeuristic(bestVar)
      val alternatives = Branching.branch { cp.post(x === value) } { cp.post(x !== value) }
      alternatives
    }
  }
}