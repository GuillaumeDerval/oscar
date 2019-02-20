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
  val analysis = new Activity(m, "Analysis", 10)
  val design = new Activity(m, "Design", 10)
  val coding = new Activity(m, "Coding", 15)
  val test = new Activity(m, "Testing", 25)
  val qa = new Activity(m, "Quality control", 50)
  val pm = new Activity(m, "Project Management", 60)

  val activities = new BoundedArray[Activity](6, Activity.setIndex)
  activities.:::(List(qa, design, analysis, test, coding, pm))

  // Running modes
  val runningModesForAnalysisQAPM = new RunningModeResources(2)
  val rm_analysis = new RunningMode("RM_ANALYSIS", 0)
  val rm_qa_pm = new RunningMode("RM_QA_PM", 0)
  runningModesForAnalysisQAPM.addRunningMode(rm_analysis, true)
  runningModesForAnalysisQAPM.addRunningMode(rm_qa_pm)
  runningModesForAnalysisQAPM.addSetupTime(rm_analysis, rm_qa_pm, 1)
  runningModesForAnalysisQAPM.addSetupTime(rm_qa_pm, rm_analysis, 1)

  val runningModesForDevTest = new RunningModeResources(2)
  val rm_dev = new RunningMode("RM_DEV", 0)
  val rm_test = new RunningMode("RM_TEST", 0)
  runningModesForDevTest.addRunningMode(rm_dev,true)
  runningModesForDevTest.addRunningMode(rm_test)
  runningModesForDevTest.addSetupTime(rm_dev, rm_test, 2)
  runningModesForDevTest.addSetupTime(rm_test, rm_dev, 2)

  // Resources
  val res_analyst = new Resource(m, "AnalystQAPM", 5, runningModesForAnalysisQAPM)
  val res_senior_dev_test = new Resource(m, "Senior Dev/test", 2, runningModesForDevTest)

  val resources = new BoundedArray[Resource](2, Resource.setIndex)
  resources.:::(List(res_analyst, res_senior_dev_test))

  // Resource usage
  val resUsages = new ActivityResourceUsages(6, 2)  // parameters looks strange, why not activity and resource list ?
  resUsages.addActivityResourceUsage(analysis, res_analyst, rm_analysis, 2)
  resUsages.addActivityResourceUsage(design, res_analyst, rm_analysis, 1)
  resUsages.addActivityResourceUsage(coding, res_senior_dev_test, rm_dev, 2)
  resUsages.addActivityResourceUsage(test, res_senior_dev_test, rm_test, 1)
  resUsages.addActivityResourceUsage(qa, res_analyst, rm_analysis, 1)
  resUsages.addActivityResourceUsage(pm, res_analyst, rm_analysis, 1)

  // Precedences
  val precedences = new Precedences(6)
  precedences.addPrecedence(analysis, design)
  precedences.addPrecedence(design, coding)
  precedences.addPrecedence(coding, test)
  precedences.addPrecedence(analysis, qa)

  // Scheduling Problem
  val scProblem = new SchedulingProblem(m, activities, resources, precedences, resUsages)

  // Tardiness variables
  val tardinessPenalty = 100
  val activitiesTardiness = Array.tabulate(activities.size)(i => {
    scProblem.startTimes(i)*tardinessPenalty
  })
  val globalTardiness:Objective = Sum(activitiesTardiness)

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
