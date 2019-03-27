package oscar.cbls.test.scheduling

import oscar.cbls.{Objective, Store}
import oscar.cbls.business.scheduling.model.{DisjunctiveResource, Schedule}
import oscar.cbls.business.scheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

object SwDevTardiness {
  // Model
  val (a1, a2, a3, a4, a5) = (0, 1, 2, 3, 4)
  val durations = Array.tabulate(5)(_ => 10L)
  // Resource
  val analyst = new DisjunctiveResource(List(a1, a2, a3, a4, a5))

  def main(args: Array[String]): Unit = {
    // CBLS Store
    val m = new Store()
    val schedule = new Schedule(m, durations, Nil, 0 to 4, Array(analyst))
    // Tardiness variables
    val tardinessPenalty = 100L
    val activitiesTardiness = Array.tabulate(5) { i =>
      schedule.startTimes(i) * tardinessPenalty * i
    }
    val globalTardiness: Objective = Sum(activitiesTardiness)
    // Model closed
    m.close()
    // Neighborhoods
    val swapNH = new SwapActivity(schedule, "Swap")
    val reinsertNH = new ReinsertActivity(schedule, "Reinsert")
    val combinedNH = BestSlopeFirst(List(Profile(reinsertNH), Profile(swapNH)))
    // This is the search strategy
    combinedNH.doAllMoves(obj = globalTardiness)
    // And here, the results
    println(combinedNH.profilingStatistics)
    println(s"*************** RESULTS ***********************************")
    println(s"Schedule makespan = ${schedule.makeSpan.value}")
    println(s"Scheduling sequence = ${schedule.activitiesPriorList.value}")
    println("Scheduling start times = [  ")
    schedule.startTimes.foreach(v => println(s"    $v"))
    println("]")
    println(s"Global tardiness = ${globalTardiness.value}")
  }

}
