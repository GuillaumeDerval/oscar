package oscar.cbls.test.scheduling

import oscar.cbls._
import oscar.cbls.algo.boundedArray.BoundedArray
import oscar.cbls.business.seqScheduling.model._
import oscar.cbls.business.seqScheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

object SeqSchedulingWithoutResources {
  // CBLS Store
  val m = new Store()
  // Activities
  val a = new Activity(m, "Analysis", 10)
  val b = new Activity(m, "Design", 10)
  val c = new Activity(m, "Coding", 15)
  val d = new Activity(m, "Testing", 25)
  val e = new Activity(m, "Quality control", 50)
  val f = new Activity(m, "Project Management", 60)
  val activities = new BoundedArray[Activity](6, Activity.setIndex)
  activities.:::(List(a, b, c, d, e, f))
  // No resources
  val resources = new BoundedArray[Resource](0)
  val resUsages = new ActivityResourceUsages(6, 0)
  // Precedences
  val precedences = new Precedences(6)
  precedences.addPrecedence(a, b)
  precedences.addPrecedence(b, c)
  precedences.addPrecedence(c, d)
  precedences.addPrecedence(a, e)
  // Scheduling Problem
  val scProblem = new SchedulingProblem(m, activities, resources, precedences, resUsages)
  // Model closed
  m.close()

  def main(args: Array[String]): Unit = {
    // Neighborhoods
    val swapNH = new SwapActivity(scProblem, "Swap")
    val reinsertNH = new ReinsertActivity(scProblem, "Reinsert")
    val combinedNH = BestSlopeFirst(List(Profile(reinsertNH), Profile(swapNH)))
    // This is the search strategy
    combinedNH.doAllMoves(obj = scProblem.mkspObj)
    // And here, the results
    println(combinedNH.profilingStatistics)
    println(s"*************** RESULTS ***********************************")
    println(s"Schedule makespan = ${scProblem.makeSpan.value}")
    println(s"Scheduling sequence = ${scProblem.activitiesPriorList.value}")
    println("Scheduling start times = [  ")
    scProblem.startTimes.foreach(v => println(s"    $v"))
    println("]")
  }
}
