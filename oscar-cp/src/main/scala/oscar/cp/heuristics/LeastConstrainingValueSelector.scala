package oscar.cp.heuristics

import oscar.cp.core.variables.CPIntVar
import oscar.cp._

/** Least Constraining Value Selector
  * @param variables  The variables on which the value heuristic is applied
  * @param maxDomSize Limits the number of values that are considered during the selection process, by default 20
  * @author           Yannick Goulleven : yannick.goulleven@student.uclouvain.be
  */
class LeastConstrainingValueSelector(variables:Array[CPIntVar], maxDomSize:Int = 30) {

  private[this] val context = variables(0).context
  private[this] val threshold = maxDomSize

  // selects the value for a variable that removes the least values from the domains of other variables
  def selectValue(i:Int):Int = {
    var nLeft = propagate(i, variables(i).getMin)
    var bestValue = variables(i).getMin

    if(variables(i).getSize > threshold) {
      val left = propagate(i, variables(i).getMax)
      if(left > nLeft) {
        bestValue = variables(i).getMax
      }
    }
    else {
      val values = new Array[Int](variables(i).getSize)
      variables(i).fillArray(values)
      for(x <- values) {
        val left = propagate(i, x)
        if(left > nLeft) {
          bestValue = x
          nLeft = left
        }
      }
    }
    bestValue
  }

  private def propagate(i: Int, x: Int):Int = {

    context.pushState()
    val out = isInconsistent(context.assign(variables(i), x))
    val ret = if(!out) {
      var count = 0
      for(i <- variables.indices) {
        count += variables(i).getSize
      }
      count
    }
    else {
      0
    }
    context.pop()
    ret
  }

}