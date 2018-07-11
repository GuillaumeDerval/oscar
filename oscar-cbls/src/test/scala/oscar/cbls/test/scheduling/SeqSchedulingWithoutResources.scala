package oscar.cbls.test.scheduling

import oscar.cbls.business.seqScheduling.model._

object SeqSchedulingWithoutResources {
  // Scheduling model
  val schModel = new SchedulingModel(6)
  // Activities
  val a = Activity("A", 3)
  schModel.addActivity(a)
  val b = Activity("B", 4)
  schModel.addActivity(b)
  val c = Activity("C", 3)
  schModel.addActivity(c)
  val d = Activity("D", 1)
  schModel.addActivity(d)
  val e = Activity("E", 3)
  schModel.addActivity(e)
  val f = Activity("F", 3)
  schModel.addActivity(f)
  // Precedences
  schModel.addPrecedenceActivities(a, d)
  schModel.addPrecedenceActivities(a, e)
  schModel.addPrecedenceActivities(b, c)
  schModel.addPrecedenceActivities(d, f)

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
