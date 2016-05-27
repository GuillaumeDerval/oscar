package oscar.linprog.modeling

/**
 * Represents the set of constraints making the problem infeasible as found by the infeasibility analysis.
 *
 * @param linearConstraints the names of the linear constraints belonging to the infeasible set (including the indicator constraints)
 * @param lowerBounds the names of the variables whose lower bound belongs to the infeasible set
 * @param upperBounds the names of the variables whose upper bound belongs to the infeasible set
 */
case class InfeasibleSet(linearConstraints: Seq[String], lowerBounds: Seq[String], upperBounds: Seq[String]) {
  override def toString = {
    s""""
        |Constraints: ${linearConstraints.mkString(", ")}
        |Lower bounds: ${lowerBounds.mkString(", ")}
        |Upper bounds: ${upperBounds.mkString(", ")}
     """.stripMargin
  }
}

case object NoInfeasibilityFoundException extends Exception("The infeasibility analysis could not find any infeasibility.")
