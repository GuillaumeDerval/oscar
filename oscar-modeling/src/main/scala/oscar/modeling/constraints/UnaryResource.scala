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

import oscar.modeling.algebra.bool.BoolExpression
import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.algebra.integer.IntExpression

/**
  * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks (with resources(i) = id) can overlap in time
  *
  * @param starts the variables representing the start time of the tasks
  * @param durations the variables representing the duration of the tasks
  * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
  * @param resources the variables representing the resource where the task is scheduled
  * @param id, the resource on which we want to constraint, tasks i such that resources(i) != id are not considered
  * @return a constraint ensuring activities don't overlap in time
  */
case class UnaryResourceSimple(starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], resources: Array[IntExpression], id: Int = 1) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = starts ++ durations ++ ends ++ resources

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}

/**
  * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks can overlap in time
  *
  * @param starts the variables representing the start time of the tasks
  * @param durations the variables representing the duration of the tasks
  * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
  * @param types the integers representing the type of each activity that will be used as entry in the transition times matrix
  * @param transitionTimes matrix of the transition times between the different activities according to their respective type
  * @return a constraint ensuring activities don't overlap in time and that consecutive activities are separated by a transition time corresponding to their respective type
  */
case class UnaryResourceTransitionType(starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], types: Array[Int], transitionTimes: Array[Array[Int]]) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = starts ++ durations ++ ends

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}

/**
  * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks can overlap in time
  *
  * @param starts the variables representing the start time of the tasks
  * @param durations the variables representing the duration of the tasks
  * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
  * @param transitionTimes matrix of the transition times between the different activities
  * @return a constraint ensuring activities don't overlap in time and that consecutive activities are separated by their respective transition time
  */
case class UnaryResourceTransition(starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], transitionTimes: Array[Array[Int]]) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = starts ++ durations ++ ends

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}

/**
  * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks can overlap in time
  *
  * @param starts the variables representing the start time of the tasks
  * @param durations the variables representing the duration of the tasks
  * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
  * @param familyMatrix matrix of the transition times between the different families
  * @param families The family associated to each activity
  * @return a constraint ensuring activities don't overlap in time and that consecutive activities are separated by their respective transition time
  */
case class UnaryResourceTransitionFamilies(starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], familyMatrix: Array[Array[Int]], families: Array[Int]) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = starts ++ durations ++ ends

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}

object UnaryResource {
  /**
    * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks can overlap in time
    *
    * @param starts the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @return a constraint ensuring activities don't overlap in time
    */
  def apply(starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression]): Constraint = {
    val n = starts.length
    val resources = Array.fill[IntExpression](n)(0)
    UnaryResourceSimple(starts, durations, ends, resources, 0)
  }

  /**
    * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks (with required(i) = true) can overlap in time
    *
    * @param starts the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param required tells if a task is scheduled on this resource or not, if not this task is not constrained
    * @return a constraint ensuring activities don't overlap in time
    */
  def apply(starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], required: Array[BoolExpression]): Constraint = {
    UnaryResourceSimple(starts, durations, ends, required.asInstanceOf[Array[IntExpression]])
  }

  /**
    * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks can overlap in time
    *
    * @param starts the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param types the integers representing the type of each activity that will be used as entry in the transition times matrix
    * @param transitionTimes matrix of the transition times between the different activities according to their respective type
    * @return a constraint ensuring activities don't overlap in time and that consecutive activities are separated by a transition time corresponding to their respective type
    */
  def apply(starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], types: Array[Int], transitionTimes: Array[Array[Int]]): Constraint = {
    UnaryResourceTransitionType(starts, durations, ends, types, transitionTimes)
  }

  /**
    * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks can overlap in time
    *
    * @param starts the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param transitionTimes matrix of the transition times between the different activities
    * @return a constraint ensuring activities don't overlap in time and that consecutive activities are separated by their respective transition time
    */
  def apply(starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], transitionTimes: Array[Array[Int]]): Constraint = {
    UnaryResourceTransition(starts, durations, ends, transitionTimes)
  }


  /**
    * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks can overlap in time
    *
    * @param starts the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param familyMatrix matrix of the transition times between the different families
    * @param families The family associated to each activity
    * @return a constraint ensuring activities don't overlap in time and that consecutive activities are separated by their respective transition time
    */
  def apply(starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], familyMatrix: Array[Array[Int]], families: Array[Int]): Constraint = {
    UnaryResourceTransitionFamilies(starts, durations, ends, familyMatrix, families)
  }

}