package oscar.cp.searches.lns.operators

import oscar.algo.search.SearchStatistics
import oscar.cp.searches.lns.CPIntSol

/**
  * Adaptive large neighbourhood search operator with no parameter.
  *
  * @param function the function that the operator applies.
  */
class ALNSNoParamOperator(
                           name: String,
                           perfMetric: (ALNSElement, Int, SearchStatistics) => Double,
                           score: Double = 0.0,
                           rFactor: Double = 1.0,
                           failThreshold: Int,
                           val function: () => (CPIntSol => Unit, Option[Int], Option[Int])
                         ) extends ALNSOperator(name, perfMetric, score, rFactor, failThreshold){

  override def getFunction: (CPIntSol => Unit, Option[Int], Option[Int]) = function()

  override def tuneParameters(): ALNSNoParamOperator = this

  override def nParamVals = 1
}
