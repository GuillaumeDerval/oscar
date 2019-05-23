package oscar.cp.heuristics

import oscar.cp.CPIntVar
import oscar.cp.core.CPSolver
import oscar.cp._


/** Bound-Impact Value Selector from "Making the first solution good!"
  *
  * @param cp         The  cp solver
  * @param variables  The variables on which the value heuristic is applied
  * @param maxDomSize If the size of the domain of a variable exceeds this number, only the bounds are considered
  * @author           Yannick Goulleven : yannick.goulleven@student.uclouvain.be
  */
class BoundImpactValueSelector(cp: CPSolver, variables: Array[CPIntVar], maxDomSize: Int= 30) {

  private[this] val isMinimization:Boolean = cp.objective.objs.head.isMin
  private[this] val context = variables(0).context
  private[this] val threshold = maxDomSize
  private[this] val isDecisionVars = Array.fill(variables.length)(false)
  private[this] val domB = cp.objective.objs.head.domBest

  def selectValue(i: Int): Int = {

    var bestBound = bound(i, variables(i).getMin)
    var bestValue = variables(i).getMin

    // select maxDomsize variables randonly
    if(variables(i).getSize > threshold) {
      val b = bound(i, variables(i).getMax)
      if(b < bestBound) {
        bestValue = variables(i).getMax
        bestBound = b
      }
    }
    else {
      val values = new Array[Int](variables(i).getSize)
      variables(i).fillArray(values)

      for(x <- values) {
        val b = bound(i, x)
        if(b < bestBound) {
          bestValue = x
          bestBound = b
        }
      }
    }
    bestValue
  }

  def isDecisionVar:Array[Boolean] = {
    isDecisionVars
  }

  private def bound(i: Int, x: Int): Int = {
    context.pushState()
    val out = isInconsistent(context.assign(variables(i), x))
    val ret = if(!out) {
      val db = cp.objective.objs.head.domBest
      if(db != domB) isDecisionVars(i) = true
      if(isMinimization) db
      else -db
    }
    else {
      Integer.MAX_VALUE
    }
    context.pop()
    ret
  }
}
