package oscar.modeling.solvers

trait Solve[RetVal] {
  def onSolution: () => RetVal
  def onSolution(o: => RetVal) = onSolutionF(() => o)
  def onSolutionF(o: () => RetVal)
}

/**
  * Either proxy the calls to onSolution if to is of class Solve[RetVal], or store them locally
  * @tparam RetVal
  */
abstract class SolveHolder[RetVal] extends Solve[RetVal] {
  protected class MinimalSolve extends Solve[RetVal] {
    private var _onSolution: () => RetVal = null
    override def onSolution: () => RetVal = _onSolution
    override def onSolutionF(o: () => RetVal): Unit = _onSolution = o
  }
  private[this] val rto = solveRedirectTo match {
    case solve: Solve[RetVal] => solve
    case _ => new MinimalSolve
  }

  def onSolution: () => RetVal = rto.onSolution
  def onSolutionF(o: () => RetVal) = rto.onSolutionF(o)
  protected val solveRedirectTo: Any
}