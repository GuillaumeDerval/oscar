package oscar.modeling.solvers.cp

import oscar.algo.search.Branching
import Branchings.{Alternative, BranchingInstantiator}
import oscar.modeling.solvers.Solve


/**
  * Contains the needed data for a simple CPSolver: branching and solution management
  *
  * @tparam RetVal
  */
trait CPSolve[RetVal] extends Solve[RetVal] {
  protected var branching: BranchingInstantiator = null

  def getSearch: BranchingInstantiator = branching
  def setSearch(b: BranchingInstantiator): Unit = branching = b
  def setSearch(b: Branching): Unit = branching = (_) => b
  def setSearch(b: => Seq[Alternative]): Unit = branching = Branchings(b)
}



