package oscar.modeling.solvers.cp

import oscar.algo.search.Branching
import Branchings.{Alternative, BranchingInstantiator}


/**
  * Contains the needed data for a simple CPSolver: branching and solution management
  *
  * @tparam RetVal
  */
trait CPSolve[RetVal] {
  protected var branching: BranchingInstantiator = null
  protected var on_solution: () => RetVal = null

  def getSearch: BranchingInstantiator = branching
  def setSearch(b: BranchingInstantiator): Unit = branching = b
  def setSearch(b: Branching): Unit = branching = (_) => b
  def setSearch(b: => Seq[Alternative]): Unit = branching = Branchings(b)

  def onSolution = on_solution
  def onSolution(o: => RetVal): Unit = on_solution = () => o
}



