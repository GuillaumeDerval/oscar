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

package oscar.modeling.algebra.integer

import oscar.modeling.algebra.Expression

import scala.collection.mutable.HashSet

/**
 * Min of an array
 */
case class Min(a: Array[IntExpression]) extends IntExpression {
  override def evaluate(): Int = a.foldLeft(Integer.MAX_VALUE)((aa: Int, ba: IntExpression) => aa min ba.evaluate())
  override def min: Int = a.foldLeft(Integer.MAX_VALUE)((aa: Int, ba: IntExpression) => aa min ba.min)
  override def max: Int = a.foldLeft(Integer.MIN_VALUE)((aa: Int, ba: IntExpression) => aa max ba.max)
  override def values(): Iterable[Int] = {
    //TODO: we can make it better easily
    val s = new HashSet[Int]()
    for(i <- a)
      s ++= i.values
    s
  }

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = a

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (Expression) => Expression): IntExpression = Min(a.map(func).asInstanceOf[Array[IntExpression]])

  /**
    * True if the variable is bound
    */
  override def isBound: Boolean = subexpressions().forall(_.isBound)
}
