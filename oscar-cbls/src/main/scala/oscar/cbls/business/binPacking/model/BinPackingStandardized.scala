/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
/**
 * ****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Gaël Thouvenin
 * ****************************************************************************
 */

package oscar.cbls.business.binPacking.model

import oscar.cbls.core.computation.Domain.rangeToDomain
import oscar.cbls.core.computation.IntValue.int2IntValue
import oscar.cbls.core.computation.{CBLSIntVar, CBLSSetVar, IntValue, Store}
import oscar.cbls.core.constraint.{Constraint, ConstraintSystem}
import oscar.cbls.lib.constraint.LE
import oscar.cbls.lib.invariant.logic.DenseCluster
import oscar.cbls.lib.invariant.set.SetSum

/**
 * @author gael.thouvenin@student.umons.ac.be
 */
case class BinPackingStandardized(itemSizes: Array[Int], binSizes: Array[Int], initialBin: Int)(implicit s: Store) {
  val itemsCount = itemSizes.size

  val binsCount = binSizes.size

  val itemsRange = 0 until itemsCount

  val binsRange = 0 until binsCount

  /**
   * Assigned bin for each item -> the array of CBLSIntVar to apply the Neighborhoods on
   */
  val itemAssignments = Array.tabulate(itemsCount)(x =>
    CBLSIntVar(s, initialBin, binsRange, "Bin of item %d".format(x)))

  /**
   * Assigned items for each bin
   */
  val binAssignments = DenseCluster(itemAssignments, Array.tabulate(binsCount)(x =>
    CBLSSetVar(s,itemsRange, name = "Contents of bin %d".format(x)))).clusters

  /**
   * Amount of space used for each bin
   */
  val binUseAmounts = (for (i <- binsRange) yield SetSum(binAssignments(i), itemSizes(_))).toArray

  /**
   * Posts the binPacking Constraint
   * @param c the constraintSystem
   * @param filter index => boolean : a function that returns true if a constraint should be applied on the given bin, false otherwise
   */
  def withSizeConstraints(filter: Int => Boolean = _ => true, weight: IntValue = 1)(implicit c: ConstraintSystem) = {
    val cs = new ConstraintSystem(s)
    for (bId <- binsRange if filter(bId)) cs.post(LE(binUseAmounts(bId), binSizes(bId)))
    cs.close()
    c.post(cs, weight.value)
    for (assignation <- itemAssignments) c.violation(assignation)
  }

  /**
   * Posts a Constraint for each bin
   * @param fct (index, assigned items) => Constraint : a function that gives a constraint for the bin, according to
   *            it's index and it's contents or None if the bin shouldn't be constrained
   * @param c the constraintSystem
   * @return
   */
  def withConstraintsOnBins(fct: (Int, CBLSSetVar) => Option[Constraint], weight: IntValue = 1)(implicit c: ConstraintSystem): Unit = {
    val cs = ConstraintSystem(s)
    for (bId <- binsRange) {
      val optConst = fct(bId, binAssignments(bId))
      optConst match {
        case Some(const) => cs.post(const)
        case None => ()
      }
    }
    cs.close()
    c.post(cs, weight.value)
    for (i <- itemAssignments) c.violation(i)
  }

  /**
   * Posts a Constraint for each item
   * @param fct (index, assignedBin) => Constraint : a function that gives a constraint for the item, according to it's
   *            index and it's assignation bin or None if the item shouldn't be constrained
   * @param c the constraintSystem
   * @return
   */
  def withConstraintOnItems(fct: (Int, CBLSIntVar) => Option[Constraint], weight: IntValue = 1)(implicit c: ConstraintSystem): Unit = {
    val cs = ConstraintSystem(s)
    for (iId <- itemsRange) {
      val optConst = fct(iId, itemAssignments(iId))
      optConst match {
        case Some(const) => cs.post(const)
        case None => Nil
      }
    }
    cs.close()
    c.post(cs, weight)
    for (i <- itemAssignments) c.violation(i)
  }
}

/**
 * @author gael.thouvenin@student.umons.ac.be
 */
object BinPackingStandardized {
  def apply(itemSizes: Traversable[Int], binSizes: Traversable[Int], initialBin: Int = 0)(implicit s: Store) = {
    new BinPackingStandardized(itemSizes.toArray, binSizes.toArray, initialBin)(s)
  }
}
