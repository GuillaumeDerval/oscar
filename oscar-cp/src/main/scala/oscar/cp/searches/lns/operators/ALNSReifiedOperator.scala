package oscar.cp.searches.lns.operators

import oscar.algo.search.SearchStatistics
import oscar.cp.searches.lns.CPIntSol

/**
  * Provides a reification of a parametrised operator with fixed values for it's parameters. The update and setActive
  * functions will modify the original operator. Should be used in a loose context.
  *
  * @param name the name of the reified operator (should be unique).
  * @param failThreshold the failThreshold of the reified operator (should be set to the minimum of the fail thresholds
  *                      of the fixed parameters).
  * @param function applies the function of the original operator with fixed parameter values.
  * @param updateFunction updates the original operator.
  * @param activationFunction activates/deactivates the original operator.
  */
class ALNSReifiedOperator(
                           name:String,
                           failThreshold: Int,
                           function: CPIntSol => Unit,
                           val updateFunction: (Int, SearchStatistics, Boolean) => Unit,
                           val activationFunction: (Boolean) => Unit
                     ) extends ALNSNoParamOperator(name, failThreshold, function){

  override def update(costImprovement: Int, stats: SearchStatistics, fail: Boolean): Unit = updateFunction(costImprovement, stats, fail)

  override def setActive(state: Boolean): Unit = activationFunction(state)

}
