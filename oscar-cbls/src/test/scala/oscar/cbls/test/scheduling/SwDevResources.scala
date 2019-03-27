package oscar.cbls.test.scheduling

import oscar.cbls.Store
import oscar.cbls.business.scheduling.model._
import oscar.cbls.business.scheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

object SwDevResources {
  // Model
  // Activities
  val (analysis, design, coding, testing, qc, pm) = (5, 4, 3, 2, 1, 0)
  val durations = Array(60L, 50L, 25L, 15L, 10L, 10L)
  val precPairs = List((analysis, design), (analysis, qc), (design, coding), (coding, testing))
  // Resources
  val (analyst_mode, qapm) = (0, 1)
  val (dev, test) = (0, 1)
  val analyst_st = SetupTimes(analyst_mode,
    Map(analysis->analyst_mode, design->analyst_mode, qc->qapm, pm->qapm),
    Map((analyst_mode, qapm)->1L, (qapm, analyst_mode)->1L))
  val analyst = new CumulativeMultiResourceWithSetupTimes(5,
    Map(analysis->2L, design->1L, qc->1L, pm->2L),
    analyst_st)
  val senior_dev_test_st = SetupTimes(dev,
    Map(coding->dev, testing->test),
    Map((dev, test)->2L, (test, dev)->2L))
  val senior_dev_test = new CumulativeMultiResourceWithSetupTimes(2,
    Map(coding->2L, testing->1L),
    senior_dev_test_st)

  def main(args: Array[String]): Unit = {
    val m = new Store()
    val schedule = new Schedule(m, durations, precPairs, 0 to 5, Array(analyst, senior_dev_test))
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
    println(s"Scheduling sequence = ${schedule.activitiesPriorList.value}")
    println("Scheduling start times = [  ")
    schedule.startTimes.foreach(v => println(s"    $v"))
    println("]")
  }
}
