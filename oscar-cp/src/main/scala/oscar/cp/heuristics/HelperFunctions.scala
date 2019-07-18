package oscar.cp.heuristics

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import util.control.Breaks._

object HelperFunctions {

  private[this] val rand = new Random(42)

  // values of the student distribution for a 95% confidence interval for a two-sided test
  private[this] val distribution = Array(
    999.99,
    12.706, 4.303, 3.182, 2.776, 2.571,
    2.447, 2.365, 2.306, 2.262, 2.228,
    2.201, 2.179, 2.160, 2.145, 2.131,
    2.120, 2.110, 2.101, 2.093, 2.086,
    2.080, 2.074, 2.069, 2.064, 2.060,
    2.056, 2.052, 2.048, 2.045, 2.042,
    2.040, 2.037, 2.035, 2.032, 2.030,
    2.028, 2.026, 2.024, 2.023, 2.021,
    2.000, 1.990, 1.984, 1.980, 1.977,
    1.975, 1.973, 1.972, 1.969, 1.960
  )

  private[this] val duration = Array(
    0L,
    1000000000L,
    2000000000L,
    3000000000L,
    4000000000L,
    5000000000L,
    6000000000L,
    7000000000L,
    8000000000L,
    9000000000L,
    10000000000L,
    11000000000L,
    12000000000L
  )

  /**
    * @param index  : the number of observations
    * @return       : the student distribution of index for a two sided test with 95% confidence
    **/
  def student(index:Int):Double = {
    if(index < 41) distribution(index-1)
    else if (index < 61) distribution(40)
    else if (index < 81) distribution(41)
    else if (index < 101) distribution(42)
    else if (index < 121) distribution(43)
    else if (index < 141) distribution(44)
    else if (index < 161) distribution(45)
    else if (index < 181) distribution(46)
    else if (index < 201) distribution(47)
    else if (index < 251) distribution(48)
    else distribution(49)
  }

  /**
    * @param futureVars   : the set of indices of un-assigned variables
    * @param varH1        : the first variable heuristic where the score returned needs to be maximized
    * @param varH2        : the second variable heuristic where the score returned needs to be maximized
    * @return             : the set of indices that maximize varH1 and ties where ties are broken maximizing varH2
    **/
  def tieBreaker(futureVars: Array[Int], varH1:Int => Double, varH2:Int => Double): Array[Int] = {
    var candidates1 = ArrayBuffer[Int]()
    var candidates2 = ArrayBuffer[Int]()
    var bestScore = Double.MinValue
    for(i <- futureVars) {

      if(varH1(i) > bestScore) {
        bestScore = varH1(i)
        candidates1 = ArrayBuffer[Int](i)
      }
      else if(varH1(i) == bestScore) {
        candidates1 += i
      }
    }
    bestScore = Double.MinValue
    if(candidates1.length > 1) {
      for(j <- candidates1) {
        if(varH2(j) > bestScore) {
          bestScore = varH2(j)
          candidates2 = ArrayBuffer[Int](j)
        }
        else if(varH2(j) == bestScore) {
          candidates2 += j
        }
      }
    }
    else{
      return candidates1.toArray
    }
    candidates2.toArray
  }

  /**
    * @param importance   : the importance in [0, 1] given to each variable heuristic -> equal importance by default
    * @param varH1        : the first variable heuristic that returns the scaled score for each variable i
    * @param varH2        : the second variable heuristic that returns the scaled score for variable i
    * @return             : the sum o both heuristics weighted by the importance
    **/
  def scaledSum(importance:Double=0.5, varH1:Int => Double, varH2:Int => Double)(i:Int):Double = {
    importance*varH1(i) + (1-importance)*varH2(i)
  }



  // helper function to compute the paretoFront
  private def dominates(i:Int, j:Int, varH1:Int => Double, varH2: Int => Double):Boolean = {
    if(varH1(i) >= varH1(j) && varH2(i) > varH2(j)) true
    else if(varH2(i) >= varH2(j) && varH1(i) > varH1(j)) true
    else false
  }

  /**
    * @param futureVars   : the set of indices of un-assigned variables
    * @param varH1        : the first variable heuristic where the score returned needs to be maximized
    * @param varH2        : the second variable heuristic where the score returned needs to be maximized
    * @return             : the set of pareto optimal variables
    **/
  def paretoFront(futureVars: Array[Int], varH1:Int => Double, varH2:Int => Double):Array[Int] = {

    var parFront = ArrayBuffer[Int]()

    for(i <- futureVars) {
      var isPareto = true
      val toRemove = ArrayBuffer[Int]()
      breakable {
        for (j <- parFront) {
            if(dominates(i, j, varH1, varH2)) {
              toRemove += j
            }
            if (dominates(j, i, varH1, varH2)) {
              isPareto = false
            }
            break
        }
      }
      for(v <- toRemove) {
        parFront -= v
      }
      if(isPareto) {
        parFront += i
      }
    }
    parFront.toArray
  }

  /**
    * @param minTime   : the minimum duration of the luby sequence
    * @param maxTime   : the maximum duration of the luby seqeuence
    * @param nTimeOuts : the number of timeouts without finding a better solution during the usage of the luby sequence
    * @return          : a random duration between minTime and maxTime
    **/
  def luby(minTime:Int, maxTime:Int)(nTimeOuts:Int): Long = {

    val m = rand.nextInt((maxTime - minTime / 2) + 1)
    val p = rand.nextInt(nTimeOuts+1)
    val r = rand.nextInt(maxTime - minTime + 1) + minTime
    val res = r - m + p
    if(res <= minTime) {
      duration(minTime)
    }
    else if(res >= maxTime) {
      duration(maxTime)
    }
    else {
      duration(res)
    }
  }

}
