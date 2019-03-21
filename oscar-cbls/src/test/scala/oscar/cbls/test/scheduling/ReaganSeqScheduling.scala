package oscar.cbls.test.scheduling

import oscar.cbls.business.seqScheduling.model._
import oscar.cbls.business.seqScheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.core.computation.Store
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

object ReaganSeqScheduling {
  // Model
  // Activities
  lazy val eat = Activity_B("Eat", 2)
    .withResource(reagan, 2)
    .precedes(sleep)
  lazy val sleep = Activity_B("Sleep", 8).withResource(reagan, 3)
  lazy val think = Activity_B("Think", 12)
    .withResource(reagan, 1)
    .precedes(drink)
  lazy val chew = Activity_B("Chew", 3)
    .withResource(reagan, 1)
    .precedes(speak)
  lazy val speak = Activity_B("Speak", 3).withResource(reagan, 3)
  lazy val drink = Activity_B("Drink", 2).withResource(reagan, 3)
  // Resource
  lazy val reagan = Resource_B("Ronald Reagan", 3)

  def main(args: Array[String]): Unit = {
    // Problem model
    val model = Store(checker = None, noCycle=false)
    val scProblem = new SchedulingProblem_B(model, Set(eat, sleep, think, chew, speak, drink), Set(reagan))
    model.close()
    println("Model closed.")
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
    println(s"Scheduling setup times: ${scProblem.setupTimes}")
  }

  /*
  val model = Store(checker = None, noCycle=false)
  // Activities
  val eat = new Activity(model, "Eat", 2)
  val sleep = new Activity(model, "Sleep", 8)
  val think = new Activity(model, "Think", 12)
  val chew = new Activity(model, "Chew", 3)
  val speak = new Activity(model, "Speak", 3)
  val drink = new Activity(model, "Drink", 2)
  val activities = new BoundedArray[Activity](6, Activity.setIndex)
  activities.:::(List(eat, sleep, think, chew, speak, drink))
  // Reagan Resource
  // Default Running Mode
  val defaultRM = new RunningMode("Default", 0)
  val reaganRMs = new RunningModeResources(1)
  reaganRMs.addRunningMode(defaultRM)
  val reagan = new Resource(model, "Reagan", 3, reaganRMs)
  val resources = new BoundedArray[Resource](1, Resource.setIndex)
  resources :+ reagan
  // Precedences
  val precedences = new Precedences(6)
  precedences.addPrecedence(think, drink)
  precedences.addPrecedence(eat, sleep)
  precedences.addPrecedence(chew, speak)
  // Resource usages
  val resUsages = new ActivityResourceUsages(6, 1)
  resUsages.addActivityResourceUsage(eat, reagan, defaultRM, 2)
  resUsages.addActivityResourceUsage(sleep, reagan, defaultRM, 3)
  resUsages.addActivityResourceUsage(chew, reagan, defaultRM, 1)
  resUsages.addActivityResourceUsage(think, reagan, defaultRM, 1)
  resUsages.addActivityResourceUsage(speak, reagan, defaultRM, 3)
  resUsages.addActivityResourceUsage(drink, reagan, defaultRM, 3)
  // Scheduling Problem
  val scProblem = new SchedulingProblem(model, activities, resources, precedences, resUsages)
  // Model closed
  model.close()
  println("Model closed.")

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
    println(s"Scheduling setup times: ${scProblem.setupTimes}")
  }
  */
}
