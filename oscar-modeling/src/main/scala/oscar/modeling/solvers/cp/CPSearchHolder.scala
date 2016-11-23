package oscar.modeling.solvers.cp

import oscar.algo.search.Branching
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.solvers.cp.Branchings.{Alternative, BranchingInstantiator}

/**
  * Stores a CP search, or redirect to the Model Declaration if it is an instance of CPSolve
  */
trait CPSearchHolder {
  val md: ModelDeclaration
  private var _cpSearch: BranchingInstantiator = null
  def getCPSearch: BranchingInstantiator = if(_cpSearch == null && md.isInstanceOf[CPSolve[_]]) md.asInstanceOf[CPSolve[_]].getSearch else _cpSearch
  def setCPSearch(b: Branching): Unit = _cpSearch = (_) => b
  def setCPSearch(b: => Seq[Alternative]): Unit = _cpSearch = Branchings(b)
  def setCPSearch(b: BranchingInstantiator): Unit = _cpSearch = b
}
