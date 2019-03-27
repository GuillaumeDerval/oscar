package oscar.cbls.test.scheduling

import oscar.cbls.Store
import oscar.cbls.business.scheduling.model._
import oscar.cbls.business.scheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.propagation.ErrorChecker
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

/**
  * This is the scheduling example in Cédric Pralet's paper
  */
object Pralet {
  // Model
  // Activities
  val (a, b, c, d, e) = (0, 1, 2, 3, 4)
  val durations = Array(4L, 1L, 3L, 4L, 2L)
  val precPairs = List((a, c), (c, e))
  // Resources
  val setupTimesR = SetupTimes(1, Map(a->1, b->1, c->1, d->1, e->0), Map((0,1)->1L, (1,0)->1L))
  val r = new CumulativeResourceWithSetupTimes(3L, Map(a->2L, b->1L, c->1L, d->2L, e->2L), setupTimesR)
  val setupTimesR1 = SetupTimes(0, Map(b->0, c->1, e->0), Map((0,1)-> 1L, (1,0)->4L))
  val r1 = new DisjunctiveResourceWithSetupTimes(List(b, c, e), setupTimesR1)

  def main(args: Array[String]): Unit = {
    val m = new Store(checker = Some(ErrorChecker()))
    val schedule = new Schedule(m, durations, precPairs, 0 to 4, Array(r, r1))
    val objFunc = Objective(schedule.makeSpan)
    m.close()
    println("Model closed.")
    // Neighborhoods
    val swapNH = new SwapActivity(schedule, "Swap")
    val reinsertNH = new ReinsertActivity(schedule, "Reinsert")
    val combinedNH = BestSlopeFirst(List(Profile(reinsertNH), Profile(swapNH)))
    // This is the search strategy
    combinedNH.doAllMoves(obj = objFunc)
    // And here, the results
    println(combinedNH.profilingStatistics)
    println(s"*************** RESULTS ***********************************")
    println(s"Schedule makespan = ${schedule.makeSpan.value}")
    println(s"Scheduling sequence = ${schedule.activitiesPriorList.value.toList}")
    println("Scheduling start times = [  ")
    schedule.startTimes.foreach(v => println(s"    $v"))
    println("]")
  }
}
