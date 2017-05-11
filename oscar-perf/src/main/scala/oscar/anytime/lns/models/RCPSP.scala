package oscar.anytime.lns.models

import oscar.cp._
import oscar.anytime.lns.Benchmark

class RCPSP(val instance: String, val bestObj: Int = Int.MaxValue) extends Benchmark {

  val resourceid = 1
  val (nTasks, nRes, resourcesCapacities, taskDescriptions) = RCPReader.readInstance(instance)


  val durationsData = taskDescriptions.map(_._1)
  val horizon = durationsData.sum
  val demandsData = taskDescriptions.map(_._2)
  val successorsData = taskDescriptions.map(_._3)

  val taskIds = 0 until nTasks
  val successors = Array.tabulate(nTasks)(t => successorsData(t))
  val resIds = 0 until nRes

  implicit val cp = CPSolver()
  //cp.silent=true

  //vars
  val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durations(t))
  val demands = Array.tabulate(nRes)(r => Array.tabulate(nTasks)(t => CPIntVar(demandsData(t)(r))))
  val resources = Array.tabulate(nTasks)(t => CPIntVar(resourceid))
  val capacities = Array.tabulate(nRes)(r => CPIntVar(resourcesCapacities(r)))

  // Constraints
  // -----------------------------------------------------------------------
  val makespan = maximum(ends)

  // Consistency
  for (t <- taskIds)
    cp.add(ends(t) === starts(t) + durations(t))

  // Cumulative
  for (r <- resIds) {
    add(maxCumulativeResource(starts, durations, ends, demands(r), resources, capacities(r), 1), Weak)
  }

  //Precedence
  for (t <- taskIds; succ <- successors(t))
    cp.add(ends(t) <= starts(succ))

  minimize(makespan)

  override def solver: CPSolver = cp

  override def decisionVariables: Array[CPIntVar] = starts

  override def bestKnownObjective: Int = bestObj

  override def problem: String = "RCPSP"
}

import scala.io.Source

/*
 * Reader used for .rcp instances
 */

object RCPReader {

  /*nbAct nbRes
   *CapaList
   * Line : qctivity description (dur (requirementList) nbSuccessors (successorList))
   */
  def readInstance(dataFile: String) = {

    var lines = Source.fromFile(dataFile).getLines.toList.filter(_ != "")

    //first line : nbAct and nbRes
    val firstLineNumbers = lines.head.split(" ")
    val (nbAct, nbRes) = (firstLineNumbers(0).toInt - 2, firstLineNumbers(1).toInt) //we remove the 2 dummy activities
    val (actIndices, resIndices) = (0 until nbAct, 0 until nbRes)

    lines = lines.drop(1)

    //second line : resources capacities
    val secondLineNumbers = lines.head.split(" ")
    val resourcesCapacities = new Array[Int](nbRes)
    for (i <- resIndices)
      resourcesCapacities(i) = secondLineNumbers(i).toInt

    lines = lines.drop(1)

    //next lines are activities descriptions

    //skip the first dummy activity
    lines = lines.drop(1)

    val activityDescriptions = new Array[Tuple3[Int, Array[Int], Array[Int]]](nbAct)
    for (i <- actIndices) {
      val lineNumbers = lines.head.split(" ")

      val requirementArray = new Array[Int](nbRes)
      for (r <- resIndices)
        requirementArray(r) = lineNumbers(1 + r).toInt

      val successors = lineNumbers.drop(nbRes + 2).map(_.toInt - 2).filter(_ != nbAct) //remove 2 from the successors id and get rid of the dummy last activity in the successors

      //(dur (requirementArray) (successorArray)
      activityDescriptions(i) = Tuple3(lineNumbers(0).toInt, requirementArray, successors)

      //next line
      lines = lines.drop(1)
    }

    (nbAct, nbRes, resourcesCapacities, activityDescriptions)
  }

}

