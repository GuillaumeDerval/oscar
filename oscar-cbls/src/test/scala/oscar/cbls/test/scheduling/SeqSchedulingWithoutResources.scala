package oscar.cbls.test.scheduling

import oscar.cbls._
import oscar.cbls.algo.boundedArray.BoundedArray
import oscar.cbls.business.seqScheduling.model._
import oscar.cbls.business.seqScheduling.neighborhood.{ReinsertActivity, SwapActivity}

object SeqSchedulingWithoutResources {
  // CBLS Store
  val m = new Store()
  // Activities
  val a = new Activity(m, "A", 3)
  val b = new Activity(m, "B", 4)
  val c = new Activity(m, "C", 3)
  val d = new Activity(m, "D", 1)
  val e = new Activity(m, "E", 3)
  val f = new Activity(m, "F", 3)
  val activities = new BoundedArray[Activity](6, Activity.setIndex)
  activities.:::(List(a, b, c, d, e, f))
  // No resources
  val resources = new BoundedArray[Resource](0)
  val resUsages = new ActivityResourceUsages(6, 0)
  // Precedences
  val precedences = new Precedences(6)
  precedences.addPrecedence(a, d)
  precedences.addPrecedence(a, e)
  precedences.addPrecedence(b, c)
  precedences.addPrecedence(d, f)
  // Scheduling Problem
  val scProblem = new SchedulingProblem(m, activities, resources, precedences, resUsages)
  // Model closed
  m.close()

  def main(args: Array[String]): Unit = {
    // Neighborhood
    //val neighborhood = new SwapActivity(scProblem, "Swap")
    val neighborhood = new ReinsertActivity(scProblem, "Reinsert")
    // This is the search strategy
    neighborhood.doAllMoves(obj = scProblem.mkspObj)
    // And here, the results
    println(s"*************** RESULTS ***********************************")
    println(s"Schedule makespan = ${scProblem.makeSpan.value}")
    println(s"Scheduling sequence = ${scProblem.activitiesPriorList.value}")
    println("Scheduling start times = [  ")
    scProblem.startTimes.foreach(v => println(s"    $v"))
    println("]")
  }
}
