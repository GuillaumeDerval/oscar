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

package oscar.modeling.vars.mip

import de.xypron.linopt.Problem
import oscar.algo.search.IntConstrainableContext
import oscar.modeling.models.mip.MIPModel
import oscar.modeling.vars.IntVarImplem

import scala.util.Random

/**
 * An integer ("discrete") variable for the MIP Solver
 */
class MIPIntVar(baseMin: Int, baseMax: Int, override val name: String, mIPModel: MIPModel) extends MIPVar with IntVarImplem {
  val realMipVar: Problem#Column = mIPModel.linProblem.column(name).`type`(Problem.ColumnType.INTEGER).bounds(baseMin.toDouble, baseMax.toDouble)

  override def context: IntConstrainableContext = null

  /**
   * @return true if the domain of the variable has exactly one value,
   *         false if the domain has more than one value
   */
  override def isBound: Boolean = mIPModel.hasSolution

  /**
   * Return a *lower bound* for this expression
   */
  override def min: Int = if(isBound) value() else realMipVar.getLowerBound.doubleValue().floor.toInt

  /**
   * Return a *higher bound* for this expression
   */
  override def max: Int = if(isBound) value() else realMipVar.getUpperBound.doubleValue().ceil.toInt

  /**
   * Test if a value is in the domain
   *
   * @param value : value to test
   * @return true if the domain contains the value val, false otherwise
   */
  override def hasValue(value: Int): Boolean = value >= min && value <= max

  /**
   * @return returns the set this variable represents, if it is bound
   */
  def value(): Int = realMipVar.getValue.toDouble.round.toInt

  /**
   * @param value
   * @return the smallest value > val in the domain
   */
  override def valueAfter(value: Int): Int = value+1

  /**
   * @param value
   * @return the largest value < val in the domain
   */
  override def valueBefore(value: Int): Int = value-1

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  override def randomValue(rand: Random): Int = rand.nextInt(max-min)

  override def iterator: Iterator[Int] = (min to max).iterator
}

object MIPIntVar {
  def apply(min: Int, max: Int, name: String)(implicit mIPModel: MIPModel): MIPIntVar = {
    new MIPIntVar(min, max, name, mIPModel)
  }
}