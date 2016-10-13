package oscar.modeling.solvers.cp

import oscar.modeling.solvers.cp.decompositions.DecompositionStrategy


/**
  * Allow to decompose into subproblems for solving in a distributed environment
  *
  * @tparam RetVal
  */
trait DecomposedCPSolve[RetVal] extends CPSolve[RetVal] {
  private var decomposition_strategy: DecompositionStrategy = null
  def setDecompositionStrategy(d: DecompositionStrategy): Unit = decomposition_strategy = d
  def getDecompositionStrategy: DecompositionStrategy = decomposition_strategy

//  override def onSolution(o: => RetVal): Unit = onSolution(o)
//  def onSolution(o: NullarySpore[RetVal]): Unit = on_solution = () => o()
}