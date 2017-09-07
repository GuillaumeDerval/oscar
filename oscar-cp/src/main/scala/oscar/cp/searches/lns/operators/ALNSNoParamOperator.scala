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
                           val function: () => (CPIntSol => Unit, Option[Int], Option[Int])
                         ) extends ALNSOperator(name, failThreshold){

  override def getFunction: (CPIntSol => Unit, Option[Int], Option[Int]) = function()

  override def nParamVals = 1
}
