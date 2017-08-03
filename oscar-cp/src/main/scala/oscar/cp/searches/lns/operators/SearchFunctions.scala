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

  //TODO: Move this to CPIntVar?
  /**
    * Finds the median of an array of values. (In place, based on the pivot method)
    * If the array has an even number of elements, the lowest of the two elements in the middle is considered as the median
    */
  private def median(values: Array[Int]): Int = findKth(values, (values.length + 1)/2)

  /**
    * Finds the kth value of the given array. (In place, based on the pivot method)
    * @param values the array of values
    * @param k the place of the value to find
    * @return the kth value in values
    */
  private def findKth(values: Array[Int], k: Int): Int = {
    //Auxillary recursive method
    def findKthAux(li: Int, ui: Int): Int = {
      var p = li + ((ui - li) / 2) + ((ui - li) % 2) //Selecting middle element as pivot
      val pivotVal = values(p)
      var i = li

      //Partitioning:
      while(i < ui){
        if(i < p && values(i) >= pivotVal){
          //Three way swap:
          values(p) = values(i)
          values(i) = values(p-1)
          values(p-1) = pivotVal
          p -= 1
        }
        else if(i > p && values(i) < pivotVal){
          //Three way swap:
          values(p) = values(i)
          values(i) = values(p+1)
          values(p+1) = pivotVal
          p += 1
        }
        i += 1
      }

      if(p == k) values(p)
      else if(p < k) findKthAux(p, ui)
      else findKthAux(li, p)
    }
    findKthAux(0, values.length)
  }


}
