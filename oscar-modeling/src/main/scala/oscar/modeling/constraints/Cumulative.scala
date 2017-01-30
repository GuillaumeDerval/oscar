package oscar.modeling.constraints

import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.algebra.integer.IntExpression
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.vars.IntVar

/**
  * Discrete Resource constraint with minimum capacity: at any time where at least one tasks overlaps, the cumulative demands of the tasks executing on the resource id, must be >= than the capacity
  *
  * @param starts the variables representing the start time of the tasks
  * @param durations the variables representing the duration of the tasks
  * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
  * @param demands the variables representing how much each task consume of the resource
  * @param resources the variables representing the resource where the task is scheduled
  * @param capacity the capacity of the resource
  * @param id, the resource on which we want to constraint the capacity (only tasks i with resources(i) = id are taken into account)
  * @return a constraint enforcing that the load over the resource is always above/at its capacity at any point of time
  */
case class MinCumulativeResource(starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], demands: Array[IntExpression], resources: Array[IntExpression], capacity: IntExpression, id: Int) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = starts ++ durations ++ ends ++ demands ++ resources ++ Array(capacity)

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}

object MinCumulativeResource{
  /**
    * Discrete Resource constraint with maximum capacity: at any time where at least one tasks overlaps, the cumulative demands of the tasks must be >= than the capacity
    *
    * @param starts the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param demands the variables representing how much each task consume of the resource
    * @param capacity the capacity of the resource
    * @return a constraint enforcing that the load over the resource is always above/at its capacity at any point of time
    */
  def apply(starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], demands: Array[IntExpression], capacity: IntExpression)(implicit modelDeclaration: ModelDeclaration): Constraint = {
    val resources = Array.fill(starts.length)(IntVar(0))
    MinCumulativeResource(starts, durations, ends, demands, resources, capacity, 0)
  }
}

/**
  * Discrete Resource constraint with maximum capacity: at any time, the cumulative demands of the tasks executing on the resource id, must be <= than the capacity
  *
  * @param starts the variables representing the start time of the tasks
  * @param durations the variables representing the duration of the tasks
  * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
  * @param demands the variables representing how much each task consume of the resource
  * @param resources the variables representing the resource where the task is scheduled
  * @param capacity the capacity of the resource
  * @param id, the resource on which we want to constraint the capacity (only tasks i with resources(i) = id are taken into account)
  * @return a constraint enforcing that the load over the resource is always below/at its capacity at any point of time
  */
case class MaxCumulativeResource(starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], demands: Array[IntExpression], resources: Array[IntExpression], capacity: IntExpression, id: Int) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = starts ++ durations ++ ends ++ demands ++ resources ++ Array(capacity)

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}

object MaxCumulativeResource {
  /**
    * Discrete Resource constraint with maximum capacity: at any time, the cumulative demands of the tasks must be <= than the capacity
    *
    * @param starts the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param demands the variables representing how much each task consume of the resource
    * @param capacity the capacity of the resource
    * @return a constraint enforcing that the load over the resource is always below/at its capacity at any point of time
    */
  def apply(starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], demands: Array[IntExpression], capacity: IntExpression)(implicit modelDeclaration: ModelDeclaration): Constraint = {
    val resources = Array.fill(starts.length)(IntVar(0))
    MaxCumulativeResource(starts, durations, ends, demands, resources, capacity, 0)
  }
}