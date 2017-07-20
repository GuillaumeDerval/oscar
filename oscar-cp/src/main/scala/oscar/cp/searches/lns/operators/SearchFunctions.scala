package oscar.cp.searches.lns.operators

import oscar.algo.search.Branching
import oscar.cp.{CPIntVar, binaryIdx, _}

object SearchFunctions {

  def conflictOrdering(vars: Array[CPIntVar], valHeuristic: String, valLearn: Boolean): Branching =
    conflictOrderingSearch(vars, vars(_).size, setValueHeuristic(vars, valHeuristic, valLearn))

  def firstFail(vars: Array[CPIntVar], valHeuristic: String, valLearn: Boolean): Branching =
    binaryFirstFailIdx(vars, setValueHeuristic(vars, valHeuristic, valLearn))

  def lastConflict(vars: Array[CPIntVar], valHeuristic: String, valLearn: Boolean): Branching =
    binaryLastConflict(vars, vars(_).size, setValueHeuristic(vars, valHeuristic, valLearn))

  def binarySplit(vars: Array[CPIntVar], valHeuristic: String, valLearn: Boolean): Branching =
    binarySplitIdx(vars, vars(_).size, setValueHeuristic(vars, valHeuristic, valLearn))

  def extensionalOriented(vars: Array[CPIntVar], valHeuristic: String, valLearn: Boolean): Branching =
    binaryIdx(vars, i => -(vars(i).constraintDegree << 7) / vars(i).size, setValueHeuristic(vars, valHeuristic, valLearn))

  def weightedDegree(vars: Array[CPIntVar], valHeuristic: String, decayRatio: Double): Branching =
    binaryMaxWeightedDegree(vars, setValueHeuristic(valHeuristic), decayRatio)

  def setValueHeuristic(vars: Array[CPIntVar], valHeuristic: String, valLearn: Boolean): Int => Int = {
    val defValHeuris = valHeuristic match{
      case "Min" => x:Int => vars(x).min
      case "Max" => x:Int => vars(x).max
      case "Median" => x:Int => median(vars(x).toArray)
      case "Random" => x:Int => vars(x).randomValue
    }
    if (valLearn)
      learnValueHeuristic(vars, defValHeuris)
    else defValHeuris
  }

  def setValueHeuristic(heuristic: String): CPIntVar => Int = {
    heuristic match{
      case "Min" => _.min
      case "Max" => _.max
      case "Median" => x:CPIntVar => median(x.toArray)
      case "Random" => _.randomValue
    }
  }

  //TODO: Implement efficient median algorithm
  //TODO: Deal with empty domain
  //TODO: Better deal with even case (mean? lower val?)
  //TODO: Move this to CPIntVar?
  private def median(s: Seq[Int]): Int = {
    val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
    if (s.size % 2 == 0) (lower.last + upper.head) / 2 else upper.head
  }
}
