package oscar.modeling.solvers.cp

import oscar.modeling.constraints.Constraint
import oscar.modeling.models.{Maximisation, Minimisation, NoOptimisation, OptimisationMethod}

/**
  * Contains data(mainly stats) about a subproblem
  * @param cartesianProductLog Log of the cartesian product of the subproblem domains
  * @param minBound minimum bound of the subproblem (max for maximisation)
  * @param discrepancy current discrepancy at the root of the subproblem
  */
class SubproblemData(val cartesianProductLog: Double, val minBound: Int, val discrepancy: Int)
{
  def this(cartesianProductLog: Double, optimisationMethod: OptimisationMethod, discrepancy: Int = -1) = this(cartesianProductLog,
    optimisationMethod match {
      case Maximisation(v) =>
        v.max
      case Minimisation(v) =>
        v.min
      case NoOptimisation() =>
        0
    }, discrepancy)

  def this(cartesianProductLog: Double, minBound: Int) = this(cartesianProductLog, minBound, -1)
}

/**
  * A subpoblem
  * @param constraints
  * @param additionalData
  */
class SubProblem(val constraints: List[Constraint], additionalData: Map[SubProblemAdditionalData[Any], Any]) {
  def this(constraints: List[Constraint]) = this(constraints, Map())
  def getData[B](what: SubProblemAdditionalData[B]): Option[B] = additionalData.get(what).map(_.asInstanceOf[B])
  def addData[B](key: SubProblemAdditionalData[B], value: B): SubProblem = new SubProblem(constraints, additionalData.+((key, value)))
}

trait SubProblemAdditionalData[+AssociatedType]
object SubProblemMinBound extends SubProblemAdditionalData[Int] {
  def compute(optimisationMethod: OptimisationMethod): Int = {
    optimisationMethod match {
      case Maximisation(v) =>
        v.max
      case Minimisation(v) =>
        v.min
      case NoOptimisation() =>
        0
    }
  }
}
object SubProblemCartesianProductLog extends SubProblemAdditionalData[Double]
object SubProblemDiscrepancy extends SubProblemAdditionalData[Int]