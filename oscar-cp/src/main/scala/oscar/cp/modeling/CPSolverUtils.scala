package oscar.cp.modeling

import oscar.algo.search.{Alternative, Branching, SearchStatistics}
import oscar.cp.core.CPPropagStrength.Weak
import oscar.cp.core.variables.{CPBoolVar, CPIntVar}
import oscar.cp.core.{CPPropagStrength, CPSolver, Constraint}
import oscar.cp.linearizedDFS.ReplayStatistics

trait CPSolverUtils {
  
    // helper functions to model with an implicit CPSolver
  def add(constraints: Iterable[_ <: Constraint])(implicit cp: CPSolver): Unit = cp.add(constraints)

  def add(c: Constraint, propagStrengh: CPPropagStrength)(implicit cp: CPSolver): Unit = cp.add(c, propagStrengh)
  def add(c: Constraint)(implicit cp: CPSolver): Unit = cp.add(c)

  def add(c: CPBoolVar)(implicit cp: CPSolver): Unit = cp.add(c)

  def post(c: Constraint, propagStrengh: CPPropagStrength = Weak)(implicit cp: CPSolver): Unit = cp.post(c, propagStrengh)
  def post(c: Constraint)(implicit cp: CPSolver): Unit = cp.post(c)

  def search(branching: Branching)(implicit cp: CPSolver): CPSolver = cp.search(branching)

  def search(block: => Seq[Alternative])(implicit cp: CPSolver): CPSolver = cp.search(block)

  def minimize(obj: CPIntVar)(implicit cp: CPSolver): CPSolver = cp.minimize(obj)
  def maximize(obj: CPIntVar)(implicit cp: CPSolver): CPSolver = cp.maximize(obj)

  def onSolution(block: => Unit)(implicit cp: CPSolver): CPSolver = cp.onSolution(block)

  def start(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue)(implicit cp: CPSolver): SearchStatistics = {
    cp.start(nSols, failureLimit, timeLimit)
  }

  def startSubjectTo(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue)(reversibleBlock: => Unit = {})(implicit cp: CPSolver): SearchStatistics = {
    cp.startSubjectTo(nSols, failureLimit, timeLimit)(reversibleBlock)
  }

  def listen()(implicit cp: CPSolver): Unit = {
    cp.listen()
  }

  def replay(solutionVariables : Seq[CPIntVar])(implicit cp: CPSolver) : ReplayStatistics =  {
    cp.replay(solutionVariables)
  }
}