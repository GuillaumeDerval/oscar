/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

package oscar.algo.search

import java.lang.management.ManagementFactory

import oscar.algo.vars.IntVarLike

/**
  * @author Sascha Van Cauwelart
  * @author Pierre Schaus
  */
class DFSReplayer(node: DFSearchNode, decisionVariables: Seq[IntVarLike]) {

  private val timeThreadBean = ManagementFactory.getThreadMXBean()

  def replay(decisions: Array[Decision]): SearchStatistics = {
    node.resetStats()
    val nModifications = decisions.length
    var i = 0

    def panic(panicInvariant: () => Boolean) = {
      val beforePanicTime = timeThreadBean.getCurrentThreadUserTime
      while (panicInvariant() && i < decisions.size - 1) {
        i += 1
        decisions(i) match {
          case _: TrailDecision => {
            decisions(i)()
          }
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
      decisions(i) match {
        case _: AlternativeDecision => nNodes += 1
        case _: TrailDecision =>
      }

      decisions(i)() //apply the search state modification

      if (node.isFailed) {
        //node.statusBehaviourDelegate.performFailureActions()
        nBacktracks += 1
        if (i < nModifications - 1) {
          decisions(i + 1) match {
            case _: Pop =>
            case _: Push | _: AlternativeDecision => totalPanicTime += panicFail() //additional failure compared to baseline model so we enter panic mode
          }
        }
      }
      else if (decisionVariables.forall(_.isBound)) {
        //onSolutionCallBack()
        //node.statusBehaviourDelegate.performSolutionActions()
        nBacktracks += 1
        nSols += 1
        node.solFound()
        totalPanicTime += panicSolution() // if a solution is found at a higher level of the search tree than with the baseline model, some panic time must be saved
      }

      i += 1
    }

    val timeInMillis = ((timeThreadBean.getCurrentThreadUserTime - beforeSolvingTime - totalPanicTime) / math.pow(10, 6)).toLong

    new SearchStatistics(nNodes,nBacktracks,timeInMillis,true,node.time,node.maxSize,nSols)

  }

}