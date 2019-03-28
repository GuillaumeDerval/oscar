package oscar.cbls.test.scheduling

import oscar.cbls.Store
import oscar.cbls.business.scheduling.model.Schedule
import oscar.cbls.business.scheduling.neighborhood.ReplaceActivity
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.combinators.Profile

object TestReplace {
  // Model
  // Activities
  val (a, b, c, d, e, f) = (0, 1, 2, 3, 4, 5)
  val durations = Array(2L, 1L, 8L, 4L, 7L, 6L)
  val precPairs = List((a,b), (c,d), (e,f))

  def main(args: Array[String]): Unit = {
    val m = new Store()
    val schedule = new Schedule(m, durations, precPairs, List(c, d, f), Array())
    val objFunc = Objective(schedule.makeSpan)
    m.close()
    println("Model closed.")
    // Neighborhood
    val replaceNH = Profile(new ReplaceActivity(schedule, "Replace"))
    // This is the search strategy
    replaceNH.doAllMoves(obj = objFunc)
    // And here, the results
    println(replaceNH.profilingStatistics)
    println(s"*************** RESULTS ***********************************")
    println(s"Schedule makespan = ${schedule.makeSpan.value}")
    println(s"Scheduling sequence = ${schedule.activitiesPriorList.value}")
    println("Scheduling start times = [  ")
    schedule.startTimes.foreach(v => println(s"    $v"))
    println("]")
  }
}
