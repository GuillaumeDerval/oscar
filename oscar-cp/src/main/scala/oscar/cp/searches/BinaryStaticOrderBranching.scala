package oscar.cp.searches

import oscar.cp._
import oscar.algo.search.Branching
import oscar.algo.reversible.ReversibleInt

class BinaryStaticOrderBranching(variables: Array[CPIntVar], valHeuris: (Int => Int)) extends Branching {

  def this(vars: Array[CPIntVar]) = this(vars, vars(_).min)

  private[this] val store = variables(0).store
  private[this] val nVariables = variables.length
  private[this] val depthRev = new ReversibleInt(store, 0)
  private[this] var depth = 0

  final override def alternatives(): Seq[Alternative] = {
    // Cache
    depth = depthRev.value

    // Update depth 
    while (depth < nVariables && variables(depth).isBound) depth += 1

    if (depth == nVariables) noAlternative
    else {
      // Trail new depth
      depthRev.value = depth
      // Alternatives
      val variable = variables(depth)
      val value = valHeuris(depth)
      List(Decision.assign(variable, value), Decision.remove(variable, value))
    }
  }
}