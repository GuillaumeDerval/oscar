package oscar.cbls.test.scheduling

import oscar.cbls.Store
import oscar.cbls.business.scheduling.model.Schedule
import oscar.cbls.business.scheduling.neighborhood.{AddActivity, RemoveActivity}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.combinators.Profile

object TestChange {
  // Model
  // Activities
  val (a, b, c, d, e, f) = (0, 1, 2, 3, 4, 5)
  val durations = Array(5L, 3L, 8L, 4L, 7L, 6L)
  val precPairs = List((a,b), (c,d), (e,f))

  def main(args: Array[String]): Unit = {
    val m = new Store()
    val schedule = new Schedule(m, durations, precPairs, List(f, c, e), Array())
    val objFunc = Objective(schedule.makeSpan)
    m.close()
    println("Model closed.")
    // Neighborhoods
    val addNH = new AddActivity(schedule, "Add")
    val removeNH = new RemoveActivity(schedule, "Remove")
    val changeNH = Profile(removeNH andThen addNH)
    // This is the search strategy
    changeNH.doAllMoves(obj = objFunc)
    // And here, the results
    println(changeNH.profilingStatistics)
    println(s"*************** RESULTS ***********************************")
    println(s"Schedule makespan = ${schedule.makeSpan.value}")
    println(s"Scheduling sequence = ${schedule.activitiesPriorList.value}")
    println("Scheduling start times = [  ")
    schedule.startTimes.foreach(v => println(s"    $v"))
    println("]")
  }
}
