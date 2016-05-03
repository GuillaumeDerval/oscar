package oscar.cp.lineardfs

import java.lang.management.ManagementFactory
import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPIntVar

/**
 * Created by saschavancauwelaert on 01/12/14.
 */
class DFSReplayer(node: CPSolver, decisionVariables : Seq[CPIntVar]) {

  private val timeThreadBean = ManagementFactory.getThreadMXBean()

  def replay(searchStateModifications : Array[Decision]) : ReplayStatistics = {
    val nModifications = searchStateModifications.length
    var i = 0

    def panic(panicInvariant : () => Boolean) = {
      val beforePanicTime = timeThreadBean.getCurrentThreadUserTime
      while (panicInvariant() && i < searchStateModifications.size - 1) {
        i += 1
        searchStateModifications(i) match {
          case _ : ControlDecision => { searchStateModifications(i)() }
          case _ =>
        }
      }
      timeThreadBean.getCurrentThreadUserTime - beforePanicTime
    }

    def panicFail() = panic(() => node.isFailed)
    def panicSolution() = panic(() => decisionVariables.forall(_.isBound))

    var totalPanicTime = 0.0
    var nNodes = 0
    var nBacktracks = 0
    var nSols = 0
    val beforeSolvingTime = timeThreadBean.getCurrentThreadUserTime

    while (i < nModifications) {
      searchStateModifications(i) match {
        case _ : AlternativeDecision => nNodes += 1
        case _ : ControlDecision =>
      }

      searchStateModifications(i)() //apply the search state modification

      if (node.isFailed) {
        nBacktracks += 1
        if(i < nModifications - 1) {
          searchStateModifications(i + 1) match {
            case _ : Pop =>
            case _ : Push | _ : AlternativeDecision => totalPanicTime += panicFail() //additional failure compared to baseline model so we enter panic mode
          }
        }
      }
      else if (decisionVariables.forall(_.isBound)) {
        nBacktracks += 1
        nSols += 1
        node.solFound()
        totalPanicTime += panicSolution() // if a solution is found at a higher level of the search tree than with the baseline model, some panic time must be saved
      }

      i += 1
    }

    val timeInMillis = ((timeThreadBean.getCurrentThreadUserTime - beforeSolvingTime - totalPanicTime)/math.pow(10,6)).toLong
    new ReplayStatistics(nNodes, nBacktracks, timeInMillis , nSols)
  }

}

class ReplayStatistics(
                        val nNodes: Int,
                        val nBacktracks: Int,
                        val time: Long,
                        val nSols: Int) {
  override val toString: String = s"nNodes: $nNodes\nnBacktracks: $nBacktracks\ntime(ms): $time\nnSols: $nSols\n"
}
