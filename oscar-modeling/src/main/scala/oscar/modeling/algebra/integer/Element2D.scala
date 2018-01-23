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
import oscar.modeling.misc.VariableNotBoundException

import scala.collection.mutable

/**
  * Created by dervalguillaume on 6/06/16.
  */
case class Element2D(array: Array[Array[IntExpression]], idx1: IntExpression, idx2: IntExpression) extends IntExpression {
  /**
    * Evaluate this expression. All variables referenced have to be bound.
    *
    * @throws VariableNotBoundException when a variable is not bound
    * @return the value of this expression
    */
  override def evaluate(): Int = array(idx1.evaluate())(idx2.evaluate()).evaluate()

  override def min: Int = {
    //TODO: we can make it better easily
    values().min
  }
  override def max: Int = {
    //TODO: we can make it better easily
    values().max
  }

  override def values(): Iterable[Int] = {
    val s = new mutable.HashSet[Int]()
    for(i <- idx1.values())
      for(j <- idx2.values())
        s ++= array(i)(j).values
    s
  }

  /**
    * Returns an iterable that contains all sub-expressions of this expression
    */
  override def subexpressions(): Iterable[IntExpression] = array.flatten ++ Array(idx1, idx2)

  /**
    * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
    * This function should return a value that is of the class as the object that was given to it.
    */
  override def mapSubexpressions(func: (Expression) => Expression): IntExpression = Element2D(array.map(a => a.map(func).asInstanceOf[Array[IntExpression]]), func(idx1).asInstanceOf[IntExpression], func(idx2).asInstanceOf[IntExpression])

  /**
    * True if the variable is bound
    */
  override def isBound: Boolean = subexpressions().forall(_.isBound)
}

case class ElementCst2D(array: Array[Array[Int]], idx1: IntExpression, idx2: IntExpression) extends IntExpression {
  /**
    * Evaluate this expression. All variables referenced have to be bound.
    *
    * @throws VariableNotBoundException when a variable is not bound
    * @return the value of this expression
    */
  override def evaluate(): Int = array(idx1.evaluate())(idx2.evaluate())

  override def min: Int = {
    //TODO: we can make it better easily
    values().min
  }
  override def max: Int = {
    //TODO: we can make it better easily
    values().max
  }

  override def values(): Iterable[Int] = {
    val s = new mutable.HashSet[Int]()
    for(i <- array.indices)
      s ++= array(i)
    s
  }

  /**
    * Returns an iterable that contains all sub-expressions of this expression
    */
  override def subexpressions(): Iterable[IntExpression] = Array(idx1, idx2)

  /**
    * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
    * This function should return a value that is of the class as the object that was given to it.
    */
  override def mapSubexpressions(func: (Expression) => Expression): IntExpression = ElementCst2D(array, func(idx1).asInstanceOf[IntExpression], func(idx2).asInstanceOf[IntExpression])

  /**
    * True if the variable is bound
    */
  override def isBound: Boolean = subexpressions().forall(_.isBound)
}
