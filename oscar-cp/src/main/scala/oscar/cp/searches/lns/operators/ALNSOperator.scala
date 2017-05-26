package oscar.cp.searches.lns.operators

import oscar.algo.search.SearchStatistics
import oscar.cp.searches.lns.CPIntSol

/**
  * This abstract class defines an Adaptive large neighbourhood search operator.
  *
  * @param name the name of the operator.
  */
abstract class ALNSOperator(val name: String, failThreshold: Int) extends ALNSElement(failThreshold){

  /**
    * Applies the operator
    * Warning: Should only be used in a Sequential setting!
    */
  def apply(model: CPIntSol): Unit

  /**
    * Returns the operator internal function along with an identifier which can be used to specify the internal
    * parameters to update.
    * @return (id, function)
    */
  def get(): (Long, CPIntSol => Unit)

  /**
    * Updates the operators stats and eventual parameters (indicated by id) based on the improvement and
    * search statistics given.
    *
    * @param id The id of the parameter(s) to update
    */
  def update(id: Long, costImprovement: Int, stats: SearchStatistics, fail: Boolean): Unit = update(costImprovement, stats, fail)

  /**
    * Returns the number of parameter values that the operator holds.
    * @return
    */
  def nParamVals: Int

  override def setActive(state: Boolean): Unit = super.setActive(state)

  //Two operators are considered equals if their name is equal
  override def equals(obj: scala.Any): Boolean = obj match{
    case operator: ALNSOperator => name.equals(operator.name)
    case _ => false
  }

  override def toString: String = name
}
