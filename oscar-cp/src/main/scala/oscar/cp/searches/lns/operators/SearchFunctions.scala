package oscar.cp.searches.lns.operators

import oscar.algo.search.Branching
import oscar.cp.{CPIntVar, binaryIdx, _}

object SearchFunctions {

  def conflictOrdering(vars: Array[CPIntVar], valMax: Boolean, valLearn: Boolean): Branching =
    conflictOrderingSearch(vars, vars(_).size, setValueHeuristic(vars, valMax, valLearn))

  def firstFail(vars: Array[CPIntVar], valMax: Boolean, valLearn: Boolean): Branching =
    binaryFirstFailIdx(vars, setValueHeuristic(vars, valMax, valLearn))

  def lastConflict(vars: Array[CPIntVar], valMax: Boolean, valLearn: Boolean): Branching =
    binaryLastConflict(vars, vars(_).size, setValueHeuristic(vars, valMax, valLearn))

  def binarySplit(vars: Array[CPIntVar], valMax: Boolean, valLearn: Boolean): Branching =
    binarySplitIdx(vars, vars(_).size, setValueHeuristic(vars, valMax, valLearn))

  def extensionalOriented(vars: Array[CPIntVar], valMax: Boolean, valLearn: Boolean): Branching =
    binaryIdx(vars, i => -(vars(i).constraintDegree << 7) / vars(i).size, setValueHeuristic(vars, valMax, valLearn))

  def setValueHeuristic(vars: Array[CPIntVar], valMax: Boolean, valLearn: Boolean): Int => Int = {
    if (valLearn)
      learnValueHeuristic(vars, if (valMax) vars(_).max else vars(_).min)
    else if (valMax) vars(_).max else vars(_).min
  }
}
