package oscar.cbls.test.scheduling

import oscar.cbls.Store
import oscar.cbls.business.seqScheduling.invariants.StartTimesActivities
import oscar.cbls.business.seqScheduling.model._
import oscar.cbls.business.seqScheduling.neighborhood.SwapActivity
import oscar.cbls.core.propagation.ErrorChecker

object SeqScheduling {
  // CBLS store
  val m = new Store(checker = Some(new ErrorChecker()))
  // Scheduling Model
  val scModel = new SchedulingModel(5, 2, 2)
  // Activities
  val a = new Activity(m, "a", 4, scModel)
  val b = new Activity(m, "b", 1, scModel)
  val c = new Activity(m, "c", 3, scModel)
  val d = new Activity(m, "d", 4, scModel)
  val e = new Activity(m, "e", 2, scModel)
  scModel.addActivity(d)
  scModel.addActivity(e)
  scModel.addActivity(c)
  scModel.addActivity(b)
  scModel.addActivity(a)

  // Running Modes
  val rm0 = new RunningMode(m, "0", 0)
  val rm1 = new RunningMode(m, "1", 0)
  scModel.addRunningMode(rm0)
  scModel.addRunningMode(rm1)
  // Resources
  val res0 = new Resource(m, "r", 3, rm1, scModel)
  res0.addRunningMode(rm0)
  res0.setSetupTime(rm0, rm1, 1)
  res0.setSetupTime(rm1, rm0, 1)
  val res1 = new Resource(m, "r'", 1, rm0, scModel)
  res1.addRunningMode(rm1)
  res1.setSetupTime(rm0, rm1, 1)
  res1.setSetupTime(rm1, rm0, 4)
  scModel.addResource(res0)
  scModel.addResource(res1)
  // Precedences
  scModel.addPrecedence(a, c)
  scModel.addPrecedence(c, e)
  // Resource usages
  a.addResourceUsage(res0, 2, rm1)
  b.addResourceUsage(res0, 1, rm1)
  c.addResourceUsage(res0, 1, rm1)
  d.addResourceUsage(res0, 2, rm1)
  e.addResourceUsage(res0, 2, rm0)
  b.addResourceUsage(res1, 1, rm0)
  c.addResourceUsage(res1, 1, rm1)
  e.addResourceUsage(res1, 1, rm0)
  // Scheduling Solver
  val scSolver = new SchedulingSolver(m, scModel)
  // Model closed
  m.close()

  println("Model closed")
  println(s"Makespan at closing = ${scSolver.makeSpan}")

  def main(args: Array[String]): Unit = {
    // Neighborhood
    val neighborhood = new SwapActivity(scSolver, "Swap")
    // This is the search strategy
    neighborhood.doAllMoves(obj = scSolver.mkspObj)
    // And let cross fingers!
    println(s"*************** AFTER ALL ***********************************")
    println(s"Schedule makespan = ${scSolver.makeSpan.value}")
    println(s"Scheduling sequence = ${scSolver.activitiesSequence.value}")
    println("Scheduling start times = [  ")
    scSolver.startTimes.foreach(v => println(s"    $v"))
    println("]")
    println(s"Scheduling setup times: ${scSolver.setupTimes}")
  }



  /*
  // Scheduling model
  val schModel = new SchedulingModel(5)
  // Activities
  val a = Activity("A", 4)
  schModel.addActivity(a)
  val b = Activity("B", 1)
  schModel.addActivity(b)
  val c = Activity("C", 3)
  schModel.addActivity(c)
  val d = Activity("D", 4)
  schModel.addActivity(d)
  val e = Activity("E", 2)
  schModel.addActivity(e)
  // Running Modes
  val rm0: RunningMode = schModel.addRunningMode("0", 0)
  val rm1: RunningMode = schModel.addRunningMode("1", 0)
  // Resources
  val res0 = Resource(
    "r", 3, rm1, Set(rm0, rm1),
    Set(SetupTime(rm0, rm1, 1), SetupTime(rm1, rm0, 1)),
    schModel.numActivities
  )
  schModel.addResource(res0)
  val res1 = Resource(
    "r'", 1, rm0, Set(rm0, rm1),
    Set(SetupTime(rm0, rm1, 1), SetupTime(rm1, rm0, 4)),
    schModel.numActivities
  )
  schModel.addResource(res1)

  // Precedences
  schModel.addPrecedenceActivities(a, c)
  schModel.addPrecedenceActivities(c, e)

  // Resource usages
  schModel.addUsageResourceActivity(a, res0, 2, rm1)
  schModel.addUsageResourceActivity(b, res0, 1, rm1)
  schModel.addUsageResourceActivity(c, res0, 1, rm1)
  schModel.addUsageResourceActivity(d, res0, 2, rm1)
  schModel.addUsageResourceActivity(e, res0, 2, rm0)
  schModel.addUsageResourceActivity(b, res1, 1, rm0)
  schModel.addUsageResourceActivity(c, res1, 1, rm1)
  schModel.addUsageResourceActivity(e, res1, 1, rm0)

  // Model closed

  def main(args: Array[String]): Unit = {
    println(schModel)
    // Compute priority list
    val prl = schModel.getPriorityList
    println(s"Priority List = $prl")
    // Compute schedule
    val (startTimes, setupTimes) = schModel.getSchedule(prl)
    var strStartTimes = ""
    for {i <- startTimes.indices} { strStartTimes += s" Start Time for Activity ${prl(i)} = ${startTimes(i)}\n"}
    println(s"Start Times\n$strStartTimes")
    println(s"Setup Times = $setupTimes")
    println("Done.")
  }
  */
}
