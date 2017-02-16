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

package oscar.modeling.constraints

import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.algebra.integer.IntExpression

/**
  * Table constraint
 *
  * @param X
  * @param table
  */
case class Table(X: Array[IntExpression], table: Array[Array[Int]], starred: Option[Int]) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = X

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}

case class NegativeTable(X: Array[IntExpression], table: Array[Array[Int]], starred: Option[Int]) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = X

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}

object Table {
  def apply(X: Array[IntExpression], table: Array[Array[Int]]): Table = {
    Table(X, table, None)
  }

  def apply(x1: IntExpression, x2: IntExpression, tuples: Iterable[(Int, Int)]): Table = {
    Table(Array(x1, x2), tuples.map(t => Array(t._1, t._2)).toArray)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, tuples: Iterable[(Int, Int, Int)]): Table = {
    Table(Array(x1, x2, x3), tuples.map(t => Array(t._1, t._2, t._3)).toArray)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, x4: IntExpression, tuples: Iterable[(Int, Int, Int, Int)]): Table = {
    Table(Array(x1, x2, x3, x4), tuples.map(t => Array(t._1, t._2, t._3, t._4)).toArray)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, x4: IntExpression, x5: IntExpression, tuples: Iterable[(Int, Int, Int, Int, Int)]): Table = {
    Table(Array(x1, x2, x3, x4, x5), tuples.map(t => Array(t._1, t._2, t._3, t._4, t._5)).toArray)
  }

  def apply(x1: IntExpression, x2: IntExpression, tuples: Iterable[(Int, Int)], starred: Option[Int]): Table = {
    Table(Array(x1, x2), tuples.map(t => Array(t._1, t._2)).toArray, starred)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, tuples: Iterable[(Int, Int, Int)], starred: Option[Int]): Table = {
    Table(Array(x1, x2, x3), tuples.map(t => Array(t._1, t._2, t._3)).toArray, starred)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, x4: IntExpression, tuples: Iterable[(Int, Int, Int, Int)], starred: Option[Int]): Table = {
    Table(Array(x1, x2, x3, x4), tuples.map(t => Array(t._1, t._2, t._3, t._4)).toArray, starred)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, x4: IntExpression, x5: IntExpression, tuples: Iterable[(Int, Int, Int, Int, Int)], starred: Option[Int]): Table = {
    Table(Array(x1, x2, x3, x4, x5), tuples.map(t => Array(t._1, t._2, t._3, t._4, t._5)).toArray, starred)
  }
}

object NegativeTable {
  def apply(X: Array[IntExpression], table: Array[Array[Int]]): NegativeTable = {
    NegativeTable(X, table, None)
  }

  def apply(x1: IntExpression, x2: IntExpression, tuples: Iterable[(Int, Int)]): NegativeTable = {
    NegativeTable(Array(x1, x2), tuples.map(t => Array(t._1, t._2)).toArray)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, tuples: Iterable[(Int, Int, Int)]): NegativeTable = {
    NegativeTable(Array(x1, x2, x3), tuples.map(t => Array(t._1, t._2, t._3)).toArray)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, x4: IntExpression, tuples: Iterable[(Int, Int, Int, Int)]): NegativeTable = {
    NegativeTable(Array(x1, x2, x3, x4), tuples.map(t => Array(t._1, t._2, t._3, t._4)).toArray)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, x4: IntExpression, x5: IntExpression, tuples: Iterable[(Int, Int, Int, Int, Int)]): NegativeTable = {
    NegativeTable(Array(x1, x2, x3, x4, x5), tuples.map(t => Array(t._1, t._2, t._3, t._4, t._5)).toArray)
  }

  def apply(x1: IntExpression, x2: IntExpression, tuples: Iterable[(Int, Int)], starred: Option[Int]): NegativeTable = {
    NegativeTable(Array(x1, x2), tuples.map(t => Array(t._1, t._2)).toArray, starred)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, tuples: Iterable[(Int, Int, Int)], starred: Option[Int]): NegativeTable = {
    NegativeTable(Array(x1, x2, x3), tuples.map(t => Array(t._1, t._2, t._3)).toArray, starred)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, x4: IntExpression, tuples: Iterable[(Int, Int, Int, Int)], starred: Option[Int]): NegativeTable = {
    NegativeTable(Array(x1, x2, x3, x4), tuples.map(t => Array(t._1, t._2, t._3, t._4)).toArray, starred)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, x4: IntExpression, x5: IntExpression, tuples: Iterable[(Int, Int, Int, Int, Int)], starred: Option[Int]): NegativeTable = {
    NegativeTable(Array(x1, x2, x3, x4, x5), tuples.map(t => Array(t._1, t._2, t._3, t._4, t._5)).toArray, starred)
  }
}