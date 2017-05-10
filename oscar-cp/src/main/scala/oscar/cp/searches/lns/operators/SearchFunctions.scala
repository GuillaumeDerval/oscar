package oscar.cp.searches.lns.operators

import oscar.algo.search.Branching
import oscar.cp.CPIntVar
import oscar.cp._

object SearchFunctions {

  def conflictOrdering(vars: Array[CPIntVar], maximizeObjective: Boolean, valLearn: Boolean): Branching =
    conflictOrderingSearch(vars, vars(_).size, defaultValueHeuristic(vars, maximizeObjective, valLearn))

  def firstFail(vars: Array[CPIntVar], maximizeObjective: Boolean, valLearn: Boolean): Branching =
    binaryFirstFailIdx(vars, defaultValueHeuristic(vars, maximizeObjective, valLearn))

  def lastConflict(vars: Array[CPIntVar], maximizeObjective: Boolean, valLearn: Boolean): Branching =
    binaryLastConflict(vars, vars(_).size, defaultValueHeuristic(vars, maximizeObjective, valLearn))

  def binarySplit(vars: Array[CPIntVar], maximizeObjective: Boolean, valLearn: Boolean): Branching =
    binarySplitIdx(vars, vars(_).size, defaultValueHeuristic(vars, maximizeObjective, valLearn))

  private def defaultValueHeuristic(vars: Array[CPIntVar], maximizeObjective: Boolean, valLearn: Boolean): Int => Int = {
    if (valLearn)
      learnValueHeuristic(vars, if (maximizeObjective) vars(_).min else vars(_).max)
    else if (maximizeObjective) vars(_).min else vars(_).max
  }
}
