package oscar.cp.searches.lns.operators

import oscar.algo.search.SearchStatistics
import oscar.cp.searches.lns.CPIntSol

import scala.xml.Elem

/**
  * This abstract class defines an Adaptive large neighbourhood search operator.
  *
  * @param name the name of the operator (should be unique for operators used in the same adaptive store).
  */
abstract class ALNSOperator(
                             val name: String,
                             perfMetric: (ALNSElement, Int, SearchStatistics) => Double,
                             score: Double = 0.0,
                             rFactor: Double = 1.0,
                             failThreshold: Int
                           ) extends ALNSElement(perfMetric, score, rFactor, failThreshold){

  /**
    * Returns the operator function to apply as well as optional meta-parameter values.
    * Warning: Should only be used in a Sequential setting!
    */
  def getFunction: (CPIntSol => Unit, Option[Int], Option[Int])

  /**
    * Updates the operators stats and eventual parameters (indicated by id) based on the improvement and
    * search statistics given.
    *
    * @param id The id of the parameter(s) to update
    */
  def update(id: Long, costImprovement: Int, stats: SearchStatistics, fail: Boolean, iter: Long): Unit =
    update(costImprovement, stats, fail, iter)

  /**
    * Returns the number of active parameter values that the operator holds.
    * @return
    */
  def nParamVals: Int

  def tuneParameters(): ALNSNoParamOperator

  //Two operators are considered equals if their name is equal
  override def equals(obj: scala.Any): Boolean = obj match{
    case operator: ALNSOperator => name.equals(operator.name)
    case _ => false
  }

  override def hashCode(): Int = name.hashCode

  override def asXml(cat: String): Elem = {
    <operator>
      <name>{name}</name>
      <type>{cat}</type>
      {super.wrapStatsToXml()}
    </operator>
  }

  override def toString: String = {
    var s = "Operator:"
    s += "\n\tname: " + name
    s += "\n" + super.toString
    s
  }
}
