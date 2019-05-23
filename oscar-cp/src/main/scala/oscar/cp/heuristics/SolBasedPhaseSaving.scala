package oscar.cp.heuristics

import oscar.cp.CPIntVar

trait PhaseSaving {
  def updateSolution(): Unit
  def selectValue(i:Int):Int
}

class SolBasedPhaseSaving(variables: Array[CPIntVar], fallBack: Int => Int) extends PhaseSaving {

  private[this] val solValues = Array.ofDim[Int](variables.length)

  // updates the current best solution
  def updateSolution(): Unit = {
    for(i <- variables.indices) {
      solValues(i) = variables(i).getMin
    }
  }

  // selects the value from the previous solution if still in the domain of the variable
  // calls the fallback value heuristic otherwise
  def selectValue(i:Int):Int = {
    val r = solValues(i)
    if(variables(i).hasValue(r)) {
      r
    }
    else {
      fallBack(i)
    }
  }

}
