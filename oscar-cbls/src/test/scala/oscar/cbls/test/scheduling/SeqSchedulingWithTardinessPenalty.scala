package oscar.cbls.test.scheduling

import oscar.cbls.{Objective, Store}
import oscar.cbls.algo.boundedArray.BoundedArray
import oscar.cbls.business.seqScheduling.model._
import oscar.cbls.business.seqScheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

object SeqSchedulingWithTardinessPenalty {
  // CBLS Store
  val m = new Store()

  // Activities
  val a1 = new Activity(m, "A1", 10)
  val a2 = new Activity(m, "A2", 10)
  val a3 = new Activity(m, "A3", 10)
  val a4 = new Activity(m, "A4", 10)
  val a5 = new Activity(m, "A5", 10)

  val activities = new BoundedArray[Activity](5, Activity.setIndex)
  activities.:::(List(a1, a2, a3, a4, a5))

  // Running modes
  val runningModesForAnalysisQAPM = new RunningModeResources(1)
  val rm_analysis = new RunningMode("RM_ANALYSIS", 0)
  runningModesForAnalysisQAPM.addRunningMode(rm_analysis, true)

  // Resources
  val res_analyst = new Resource(m, "AnalystQAPM", 1, runningModesForAnalysisQAPM)

  val resources = new BoundedArray[Resource](1, Resource.setIndex)
  resources.:::(List(res_analyst))

  // Resource usage
  val resUsages = new ActivityResourceUsages(5, 1)  // parameters looks strange, why not activity and resource list ?
  resUsages.addActivityResourceUsage(a1, res_analyst, rm_analysis, 1)
  resUsages.addActivityResourceUsage(a2, res_analyst, rm_analysis, 1)
  resUsages.addActivityResourceUsage(a3, res_analyst, rm_analysis, 1)
  resUsages.addActivityResourceUsage(a4, res_analyst, rm_analysis, 1)
  resUsages.addActivityResourceUsage(a5, res_analyst, rm_analysis, 1)

  // Precedences
  val precedences = new Precedences(5) // no precedences

  // Scheduling Problem
  val scProblem = new SchedulingProblem(m, activities, resources, precedences, resUsages)

  // Tardiness variables
  val tardinessPenalty = 100
  val activitiesTardiness = Array.tabulate(activities.size)(i => {
    scProblem.startTimes(i)*tardinessPenalty*i
  })
  val globalTardiness: Objective = Sum(activitiesTardiness)

  // Model closed
  m.close()

  def main(args: Array[String]): Unit = {
    // Neighborhoods
    val swapNH = new SwapActivity(scProblem, "Swap")
    val reinsertNH = new ReinsertActivity(scProblem, "Reinsert")
    val combinedNH = BestSlopeFirst(List(Profile(reinsertNH), Profile(swapNH)))
    // This is the search strategy
    combinedNH.doAllMoves(obj = globalTardiness)
    // And here, the results
    println(combinedNH.profilingStatistics)
    println(s"*************** RESULTS ***********************************")
    println(s"Schedule makespan = ${scProblem.makeSpan.value}")
    println(s"Scheduling sequence = ${scProblem.activitiesPriorList.value}")
    println("Scheduling start times = [  ")
    scProblem.startTimes.foreach(v => println(s"    $v"))
    println("]")
    println("Setup Times:")
    println(scProblem.setupTimes)
    println(s"Global tardiness = ${globalTardiness.value}")

  }

}
