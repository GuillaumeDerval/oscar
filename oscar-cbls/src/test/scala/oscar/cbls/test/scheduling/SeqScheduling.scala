package oscar.cbls.test.scheduling

import oscar.cbls.business.seqScheduling.model._

object SeqScheduling {
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
}
