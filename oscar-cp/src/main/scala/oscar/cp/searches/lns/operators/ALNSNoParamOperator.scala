package oscar.cp.searches.lns.operators

import oscar.cp.searches.lns.CPIntSol

/**
  * Adaptive large neighbourhood search operator with no parameter.
  *
  * @param function the function that the operator applies.
  */
class ALNSNoParamOperator(
                           name: String,
                           failThreshold: Int,
                           val function: (CPIntSol) => Unit
                         ) extends ALNSOperator(name, failThreshold){

  override def apply(sol: CPIntSol): Unit = function(sol)

  override def nParamVals = 1
}
