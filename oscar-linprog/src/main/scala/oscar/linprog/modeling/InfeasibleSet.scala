package oscar.linprog.modeling

/**
 * Represents the set of constraints making the problem infeasible as found by the infeasibility analysis.
 *
 * @param linearConstraints the names of the linear constraints belonging to the infeasible set
 * @param lowerBounds the names of the variables whose lower bound belongs to the infeasible set
 * @param upperBounds the names of the variables whose upper bound belongs to the infeasible set
 */
case class InfeasibleSet(linearConstraints: Seq[String], lowerBounds: Seq[String], upperBounds: Seq[String])

case object NoInfeasibilityFoundException extends Exception("The infeasibility analysis could not find any infeasibility.")
