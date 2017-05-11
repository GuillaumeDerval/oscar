package oscar.modeling.solvers.cp.searches.alns.operators

import oscar.modeling.solvers.cp.Branchings
import oscar.modeling.solvers.cp.Branchings.BranchingInstantiator
import oscar.modeling.vars.IntVar
import oscar.modeling.vars.cp.CPIntVar

object SearchFunctions {

  def conflictOrdering(vars: Array[IntVar], maximizeObjective: Boolean, valLearn: Boolean): BranchingInstantiator =
    Branchings.conflictOrderingSearch(vars, vars(_).size, defaultValueHeuristic(vars, maximizeObjective, valLearn))

  def firstFail(vars: Array[IntVar], maximizeObjective: Boolean, valLearn: Boolean): BranchingInstantiator =
    Branchings.binaryFirstFailIdx(vars, defaultValueHeuristic(vars, maximizeObjective, valLearn))

  def lastConflict(vars: Array[IntVar], maximizeObjective: Boolean, valLearn: Boolean): BranchingInstantiator =
    Branchings.binaryLastConflict(vars, vars(_).size, defaultValueHeuristic(vars, maximizeObjective, valLearn))

  def binarySplit(vars: Array[IntVar], maximizeObjective: Boolean, valLearn: Boolean): BranchingInstantiator =
    Branchings.binarySplitIdx(vars, vars(_).size, defaultValueHeuristic(vars, maximizeObjective, valLearn))

  private def defaultValueHeuristic(vars: Array[IntVar], maximizeObjective: Boolean, valLearn: Boolean): Int => Int = {
    if (valLearn)
      Branchings.learnValueHeuristic(vars, if (maximizeObjective) vars(_).min else vars(_).max)
    else if (maximizeObjective) vars(_).min else vars(_).max
  }
}
