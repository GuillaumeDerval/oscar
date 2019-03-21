package oscar.cbls.test.scheduling

import oscar.cbls._
import oscar.cbls.business.seqScheduling.model._
import oscar.cbls.business.seqScheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

object SeqSchedulingWithoutResources {
  // Model
  lazy val analysis = Activity_B("Analysis", 10)
    .precedes(design)
    .precedes(qc)
  lazy val design = Activity_B("Design", 10)
    .precedes(coding)
  lazy val coding = Activity_B("Coding", 15)
    .precedes(testing)
  lazy val testing = Activity_B("Testing", 25)
  lazy val qc = Activity_B("Quality Control", 50)
  lazy val pm = Activity_B("Project Management", 60)

  def main(args: Array[String]): Unit = {
    // CBLS Store
    val m = new Store()
    val scProblem = new SchedulingProblem_B(m, Set(analysis, design, coding, testing, qc, pm))
    m.close()
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

  /*
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
  */
}
