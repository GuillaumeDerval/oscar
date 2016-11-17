package oscar.modeling.solvers

trait Solve[RetVal] {
  protected var on_solution: () => RetVal = null

  def onSolution = on_solution
  def onSolution(o: => RetVal): Unit = on_solution = () => o
}
