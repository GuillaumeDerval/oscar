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
//TODO set up stats redirect
class ALNSReifiedOperator(
                           name:String,
                           failThreshold: Int,
                           function: () => (CPIntSol => Unit, Option[Int], Option[Int]),
                           val updateFunction: (Long, Long, Int, Int, SearchStatistics, Boolean, Long) => Unit,
                           val activationFunction: (Boolean) => Unit
                     ) extends ALNSNoParamOperator(name, failThreshold, function){

  override def update(
                       tStart: Long,
                       tEnd: Long,
                       objStart: Int,
                       objEnd: Int,
                       iterStats: SearchStatistics,
                       fail: Boolean,
                       iter: Long
                     ): Unit = updateFunction(tStart, tEnd, objStart, objEnd, iterStats, fail, iter)

  override def setActive(state: Boolean): Unit = activationFunction(state)

}
