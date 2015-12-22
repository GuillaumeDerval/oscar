package oscar.cp.linearizedDFS

import java.io.File
import java.lang.management.ManagementFactory
import oscar.cp.linearizedDFS.utils.DecisionReader
import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPIntVar

/**
 * Created by saschavancauwelaert on 01/12/14.
 */
class DFSReplayer(node: CPSolver, decisionVariables : Seq[CPIntVar]) {

  private val timeThreadBean = ManagementFactory.getThreadMXBean()

  def replay(dirURL : String) : (Long,Int,Int,Int) = {
    var time = 0 : Long
    var nSols = 0
    var nBacktracks = 0
    var nNodes = 1
    val decisionFiles = ((new File(dirURL).listFiles.filter(_.getName.startsWith("chunk"))) map (_.getAbsolutePath) sorted)
    for (f <- decisionFiles) {
      val (timeCurrent,nSolsCurrent, nBacktracksCurrent, nNodesCurrent) : (Long,Int,Int,Int) = replay(DecisionReader.decisionsFromFile(f,decisionVariables))
      time += timeCurrent
      nSols += nSolsCurrent
      nBacktracks += nBacktracksCurrent
      nNodes += nNodesCurrent
    }
    (time,nSols,nBacktracks,nNodes)
  }

  def replay(decisionArray : Array[Array[Decision]]) : (Long,Int,Int,Int) = {
    var time = 0 : Long
    var nSols = 0
    var nBacktracks = 0
    var nNodes = 1
    for(i<- 0 until decisionArray.length) {
      val (timeCurrent,nSolsCurrent, nBacktracksCurrent, nNodesCurrent) : (Long,Int,Int,Int) = replay(decisionArray(i))
      time += timeCurrent
      nSols += nSolsCurrent
      nBacktracks += nBacktracksCurrent
      nNodes += nNodesCurrent
    }
    (time,nSols,nBacktracks,nNodes)
  }

  private def replay(searchStateModifications : Array[Decision]) : (Long,Int,Int,Int) = {
    val nModifications = searchStateModifications.length
    var i = 0

    def panic(panicInvariant : () => Boolean) = {
      val beforePanicTime = timeThreadBean.getCurrentThreadUserTime
      while (panicInvariant() && i < searchStateModifications.size - 1) {
        i += 1
        searchStateModifications(i) match {
          case Pop(_) => searchStateModifications(i)()
          case Push(_) => searchStateModifications(i)()
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
        case _ : DomainDecision => nNodes += 1
        case _ : ControlDecision =>
      }

      searchStateModifications(i)() //apply the search state modification

      if (node.isFailed) {
        nBacktracks += 1
        if(i < nModifications - 1) {
          searchStateModifications(i + 1) match {
            case Pop(_) =>
            case Push(_) | Assign(_,_) | Remove(_,_) | Fail(_) | Propagate(_) | _ : SetTimesBranch => totalPanicTime += panicFail() //additional failure compared to baseline model so we enter panic mode
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

    ((timeThreadBean.getCurrentThreadUserTime - beforeSolvingTime - totalPanicTime).toLong,nSols, nBacktracks,nNodes)
  }

}
