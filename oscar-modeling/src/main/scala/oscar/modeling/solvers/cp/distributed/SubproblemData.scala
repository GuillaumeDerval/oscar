/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.modeling.solvers.cp.distributed

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