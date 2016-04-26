/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.algo.search

import oscar.algo.reversible._
import oscar.algo.array.ArrayStack

class SearchStatistics(
  val nNodes: Int,
  val nFails: Int,
  val time: Long,
  val completed: Boolean,
  val timeInTrail: Long,
  val maxTrailSize: Int,
  val nSols: Int) {
  override val toString: String = s"nNodes: $nNodes\nnFails: $nFails\ntime(ms): $time\ncompleted: $completed\ntimeInTrail: $timeInTrail\nnSols: $nSols\n"
}

/**
 * DFS and Bounded Dicrepancy DFS
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class Search(node: SearchNode, branching: Branching) {

  private val alternativesStack = new ArrayStack[Alternatives](100)

  // Used to count the number of discrepancy
  private val discrepancy = new ReversibleInt(node, 0)

  // Actions to execute in case of solution node
  private var solutionActionsStat = List[(SearchStatistics) => Unit]()

  // Actions to execute in case of failed node
  private var failureActions = List[() => Unit]()

  /** Adds an action to execute when a failed node is found */
  def onFailure(action: => Unit) {
    failureActions = (() => action) :: failureActions
  }

  /** Adds an action to execute when a solution node is found */
  def onSolution(action: => Unit) {
    solutionActionsStat = ((s: SearchStatistics) => action) :: solutionActionsStat
  }

  /** Adds an action to execute when a solution node is found, a statistics object on the search so far is given */
  def onSolutionWithStats(action: SearchStatistics => Unit) {
    solutionActionsStat = (action) :: solutionActionsStat
  }

  @inline private def expand(): Boolean = {
    val alternatives = branching.alternatives
    if (alternatives.isEmpty) false
    else {
      alternativesStack.push(Alternatives(alternatives))
      true
    }
  }

  def solveAll(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue): SearchStatistics = {

    // Initializes the search
    node.resetStats() // resets trailing time too
    alternativesStack.clear()
    discrepancy.value = 0

    val t0 = System.currentTimeMillis()
    val maxTime = timeLimit.toLong * 1000 + t0

    var solCounter = 0
    var nbNodes = 0
    var nBkts = 0

    def stat() = new SearchStatistics(nbNodes, nFails = nBkts, time = System.currentTimeMillis() - t0, completed = alternativesStack.isEmpty, timeInTrail = node.time, maxTrailSize = node.maxSize, nSols = solCounter)

    node.pushState()

    // add initial alternatives of the root node
    if (!node.isFailed) {
      node.pushState()
      val isExpandable = expand()
      if (!isExpandable) {
        node.solFound()
        val stats = stat()
        solutionActionsStat.foreach(_(stats))
        solCounter += 1
        node.pop()
      }
    }

    while (!alternativesStack.isEmpty && solCounter < nSols && nBkts < failureLimit && System.currentTimeMillis() < maxTime) {

      nbNodes += 1

      // Expandable version
      val alternatives = alternativesStack.top

      // Discrepancy of the current node
      val d = discrepancy.value + alternatives.discrepancy

      // Last if no more alternative or if the maximal discrepancy is reached
      val alternative = alternatives.next()
      val isLast = !alternatives.hasNext || d == maxDiscrepancy

      if (!isLast) node.pushState()
      else alternativesStack.pop() // no more alternative in the sequence

      discrepancy.value = d // update the discrepancy
      alternative() // apply the alternative

      if (!node.isFailed()) {
        val isExpandable = expand()
        if (!isExpandable) {
          node.solFound()
          val stats = stat()
          solutionActionsStat.foreach(_(stats))
          solCounter += 1
          nBkts += 1
          node.pop()
        }
      } else {
        failureActions.foreach(_())
        nBkts += 1
        node.pop
      }
    }

    // Pop the remaining nodes 
    var i = alternativesStack.size
    while (i != 0) {
      node.pop
      i -= 1
    }
    node.pop()

    stat() // return the statistics of the search
  }
}