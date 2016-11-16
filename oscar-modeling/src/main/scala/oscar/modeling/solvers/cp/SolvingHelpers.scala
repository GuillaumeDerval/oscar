package oscar.modeling.solvers.cp

import java.util.concurrent.LinkedBlockingQueue

import oscar.algo.search.Branching
import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.{SPSearchStatistics, SearchStatistics}
import oscar.modeling.models.operators.ModelOperator
import oscar.modeling.models.{Model, ModelDeclaration}
import Branchings._
import oscar.modeling.solvers.cp.distributed._
import oscar.modeling.vars.IntVar

import scala.collection.mutable.ListBuffer
import scala.spores.NullarySpore

/**
  * A trait for object that would want to watch solving
  * @tparam RetVal
  */
trait Watcher[RetVal] {
  /**
    * Called when a subproblem just started
    *
    * @param spid id of the subproblem
    */
  def startedSubproblem(spid: Int): Unit

  /**
    * Called when a subproblem ends
    *
    * @param spid id of the subproblem
    * @param timeTaken time taken, in nanosecs
    * @param searchStats search stats of the subproblem
    */
  def endedSubproblem(spid: Int, timeTaken: Double, searchStats: SPSearchStatistics): Unit

  /**
    * Called when a new solution is found (during the search)
    *
    * @param solution new solution
    * @param newBound possible new bound
    */
  def newSolution(solution: RetVal, newBound: Option[Int]): Unit

  /**
    * Called when a solver give a recap of the found solution (after the search, no duplicates with newSolution)
    *
    * @param nbSolutions the number of solutions
    * @param solutions only filled if RetVal is not Unit
    */
  def solutionRecap(nbSolutions: Int, solutions: List[RetVal])

  /**
    * Called when solving is completely done and all solutions have been sent. No more function calls will be made
    * @param completed: True if the timeout was not reached
    */
  def allDone(completed: Boolean): Unit
}

/**
  * A watcher multiplexer
  * @param watchers
  * @tparam RetVal
  */
class WatcherMultiplexer[RetVal](watchers: Iterable[Watcher[RetVal]]) extends Watcher[RetVal]{
  /**
    * Called when a subproblem just started
    *
    * @param spid id of the subproblem
    */
  override def startedSubproblem(spid: Int): Unit = watchers.foreach(_.startedSubproblem(spid))

  /**
    * Called when a subproblem ends
    *
    * @param spid        id of the subproblem
    * @param timeTaken   time taken, in nanosecs
    * @param searchStats search stats of the subproblem
    */
  override def endedSubproblem(spid: Int, timeTaken: Double, searchStats: SPSearchStatistics): Unit = watchers.foreach(_.endedSubproblem(spid, timeTaken, searchStats))

  /**
    * Called when a new solution is found (during the search)
    *
    * @param solution new solution
    * @param newBound possible new bound
    */
  override def newSolution(solution: RetVal, newBound: Option[Int]): Unit = watchers.foreach(_.newSolution(solution, newBound))

  /**
    * Called when a solver give a recap of the found solution (after the search, no duplicates with newSolution)
    *
    * @param nbSolutions the number of solutions
    * @param solutions   only filled if RetVal is not Unit
    */
  override def solutionRecap(nbSolutions: Int, solutions: List[RetVal]): Unit = watchers.foreach(_.solutionRecap(nbSolutions, solutions))

  /**
    * Called when solving is completely done and all solutions have been sent. No more function calls will be made
    *
    * @param completed : True if the timeout was not reached
    */
  override def allDone(completed: Boolean): Unit = watchers.foreach(_.allDone(completed))
}

/**
  * Statistics generation
  * @tparam RetVal
  */
class StatisticsWatcher[RetVal] extends Watcher[RetVal] {
  var currentStatistics = new SearchStatistics(0, 0, 0, false, 0, 0, 0, 0)
  val results = ListBuffer[RetVal]()
  var startTime: Long = 0

  def get = (currentStatistics, results.toList)

  def start(): Unit = startTime = System.currentTimeMillis()

  override def startedSubproblem(spid: Int): Unit = {}

  override def newSolution(solution: RetVal, newBound: Option[Int]): Unit = {
    currentStatistics = new SearchStatistics(nNodes = currentStatistics.nNodes,
      nFails = currentStatistics.nFails,
      time = currentStatistics.time,
      completed = false,
      timeInTrail = currentStatistics.timeInTrail,
      maxTrailSize = currentStatistics.maxTrailSize,
      nSols = currentStatistics.nSols+1,
      timeToLastSolution = System.currentTimeMillis()-startTime
    )
    results += solution
  }

  override def solutionRecap(nbSolutions: Int, solutions: List[RetVal]): Unit = {
    currentStatistics = new SearchStatistics(nNodes = currentStatistics.nNodes,
      nFails = currentStatistics.nFails,
      time = currentStatistics.time,
      completed = currentStatistics.completed,
      timeInTrail = currentStatistics.timeInTrail,
      maxTrailSize = currentStatistics.maxTrailSize,
      nSols = currentStatistics.nSols+nbSolutions,
      timeToLastSolution = currentStatistics.timeToLastSolution
    )
    results ++= solutions
  }

  override def allDone(completed: Boolean): Unit = {
    currentStatistics = new SearchStatistics(nNodes = currentStatistics.nNodes,
      nFails = currentStatistics.nFails,
      time = currentStatistics.time,
      completed = completed,
      timeInTrail = currentStatistics.timeInTrail,
      maxTrailSize = currentStatistics.maxTrailSize,
      nSols = currentStatistics.nSols,
      timeToLastSolution = currentStatistics.timeToLastSolution
    )
  }

  override def endedSubproblem(spid: Int, timeTaken: Double, searchStats: SPSearchStatistics): Unit = {
    currentStatistics = new SearchStatistics(nNodes = currentStatistics.nNodes+searchStats.nNodes,
      nFails = currentStatistics.nFails+searchStats.nFails,
      time = currentStatistics.time + searchStats.time,
      completed = false,
      timeInTrail = currentStatistics.timeInTrail+searchStats.timeInTrail,
      maxTrailSize = Math.max(currentStatistics.maxTrailSize, searchStats.maxTrailSize),
      nSols = currentStatistics.nSols,
      timeToLastSolution = currentStatistics.timeToLastSolution
    )
  }
}

/**
  * Proxy most functions to an underlying model
  *
  * @param md
  * @tparam CPModelType
  * @tparam Retval
  */
class ModelProxy[CPModelType <: CPSolve[Retval], Retval](md: ModelDeclaration with CPModelType) {
  implicit val modelDeclaration: ModelDeclaration with CPModelType = md

  def getCurrentModel = modelDeclaration.getCurrentModel

  def getSearch: BranchingInstantiator = md.getSearch
  def setSearch(b: Branching): Unit = md.setSearch(b)
  def setSearch(b: => Seq[Alternative]): Unit = md.setSearch(b)
  def setSearch(b: BranchingInstantiator): Unit = md.setSearch(b)

  def onSolution = md.onSolution
  def onSolution(s: => Retval): Unit = md.onSolution(s)
  def onSolution(s: NullarySpore[Retval]): Unit = md.onSolution(s())

  /**
    * Post a new constraint
    *
    * @param constraint the constraint to post
    */
  def post(constraint: Constraint): Unit = modelDeclaration.post(constraint)

  /**
    * Add a new constraint to the model
    *
    * @param constraint the constraint to add
    */
  def add(constraint: Constraint): Unit = modelDeclaration.add(constraint)

  /**
    * Apply a model operator
    *
    * @param operator operator to apply
    */
  def apply[OutputType <: Model](operator: ModelOperator[OutputType]): Unit = modelDeclaration(operator)

  /**
    * Minimize on variable v
    *
    * @param v variable to minimize
    */
  def minimize(v: IntVar) = modelDeclaration.minimize(v)

  /**
    * Maximize on variable v
    *
    * @param v variable to maximize
    */
  def maximize(v: IntVar) = modelDeclaration.maximize(v)

  /**
    * Remove the optimisation method
    */
  def removeOptimization() = modelDeclaration.removeOptimization()
}