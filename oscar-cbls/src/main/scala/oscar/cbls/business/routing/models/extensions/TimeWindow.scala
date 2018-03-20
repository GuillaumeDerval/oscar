package oscar.cbls.business.routing.models.extensions

/**
  * This class is only used to simplify the constraints creation.
  * The invariant used to set the time constraint is not a time dedicated invariant
  * so we need some extra information like earlylines, deadlines...
  *
  * This class serve as a data package
  *
  * @param earlylines For each node the time after which we can start our tasks
  * @param deadlines For each node the time before which the task has to be finished
  * @param taskDurations For each node the task's duration
  * @param maxWaitingDurations For each node the maximum among of time we can wait before starting the task.
  *                            e.g.: You can stay at a parking for a limited among of time.
  */
class TimeWindow(val earlylines: Array[Int],
                 val deadlines: Array[Int],
                 val taskDurations: Array[Int],
                 val maxWaitingDurations: Array[Int]){
}