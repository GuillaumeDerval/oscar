package oscar.modeling.solvers.cp.distributed

import java.util.concurrent.LinkedBlockingQueue

import oscar.algo.search.Branching
import oscar.cp.constraints.CPObjectiveUnit
import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.SPSearchStatistics
import oscar.modeling.models.Model
import oscar.modeling.solvers.cp.{Watcher, WatcherMultiplexer}
import oscar.modeling.solvers.cp.Branchings._
import oscar.modeling.vars.IntVar

/**
  * Message
  */
trait SolvingMessage
trait WatcherMessage extends SolvingMessage
trait MasterToSolverMessage extends SolvingMessage
trait MasterToMasterMessage extends SolvingMessage
trait SolverToMasterMessage extends SolvingMessage

case class AwaitingSPMessage() extends SolverToMasterMessage
case class SolutionMessage[RetVal](solution: RetVal, newBound: Option[Int]) extends SolverToMasterMessage with WatcherMessage
case class DoneMessage(spid: Int, timeTakenNS: Double, searchStats: SPSearchStatistics) extends SolverToMasterMessage with WatcherMessage
case class StartedMessage(spid: Int) extends SolverToMasterMessage with WatcherMessage

//Sent only on satisfaction problems, at the end of the computation
case class AskForSolutionRecap() extends MasterToSolverMessage
//solutions is only filled with solutions if RetVal is not Unit
case class SolutionRecapMessage[RetVal](nbSolutions: Int, solutions: List[RetVal]) extends SolverToMasterMessage with WatcherMessage

case class DoSubproblemMessage(spid: Int, sp: List[Constraint]) extends MasterToSolverMessage
case class BoundUpdateMessage(newBound: Int) extends MasterToSolverMessage
case class AllDoneMessage(completed: Boolean) extends MasterToSolverMessage with WatcherMessage

case class HelloMessage() extends MasterToSolverMessage with SolverToMasterMessage
case class ConfigMessage(forceImmediateSend: Boolean) extends MasterToSolverMessage
case class StartMessage() extends MasterToSolverMessage
case class SolveTimeout() extends MasterToMasterMessage

/**
  * Class that runs watchers, and send them the info from the output queue
  * @param watchers
  * @param outputQueue
  * @tparam RetVal
  */
class DistributedWatcherRunnable[RetVal](watchers: Iterable[Watcher[RetVal]],
                                         outputQueue: LinkedBlockingQueue[SolvingMessage]) extends WatcherMultiplexer(watchers) with Runnable {
  override def run(): Unit = {
    var done = false
    while(!done) {
      outputQueue.take() match {
        case SolutionMessage(solution: RetVal, newBound: Option[Int]) => newSolution(solution, newBound)
        case DoneMessage(spid, newtimeTaken, searchStats) => endedSubproblem(spid, newtimeTaken, searchStats)
        case StartedMessage(spid) => startedSubproblem(spid)
        case SolutionRecapMessage(nbSolutions: Int, solutions: List[RetVal]) => solutionRecap(nbSolutions, solutions)
        case AllDoneMessage(completed) =>
          done = true
          allDone(completed)
      }
    }
  }
}

/**
  * Manages the bound, for optimization problems
  */
trait BoundaryManager {
  type B
  def getBoundary(): B
  def updateBoundary(newval: B): Unit
}

/**
  * Particular BoundaryManager for integer bounds
  */
trait IntBoundaryManager extends BoundaryManager {
  type B = Int
}

/**
  * Synchronized boundary manager.
  * @param initial initial value
  */
class SynchronizedIntBoundaryManager(initial: Int) extends IntBoundaryManager {
  @volatile private var boundary = initial

  def getBoundary(): Int = boundary
  def updateBoundary(newval: Int) = boundary = newval
}

/**
  * A volatile variable that stores a boolean indicating if the current solver should close or not
  */
class SynchronizedForceClose {
  @volatile private var shouldClose = false
  def getShouldClose = shouldClose
  def close() = shouldClose = true
}

/**
  * Exception to be sent by a solver when its SynchronizedForceClose is true
  */
class ClosedByForce extends Exception

/**
  * Wrapper for a search, that ensures the solver will close when needed.
  *
  * Throw a ClosedByForce exception when the solver is closed by force.
  *
  * @param original original search
  * @param shouldClose indicates if the solver should close or not
  */
class ForceCloseSearchWrapper(original: Branching, shouldClose:SynchronizedForceClose) extends Branching {
  override def alternatives(): Seq[Alternative] = {
    original.alternatives().map((a: Alternative) => {
      () => {
        if(shouldClose.getShouldClose)
          throw new ClosedByForce
        a()
      }
    })
  }
}

/**
  * Wrapper for a search, that ensures the solver is up-to-date with the boundary and will close when needed
  *
  * Throw a ClosedByForce exception when the solver is closed by force.
  *
  * @param original original search
  * @param boundaryManager the manager for the bound
  * @param shouldClose indicates if the solver should close or not
  * @param cpobjective the cp objective to enforce
  */
class IntBoundaryUpdateSearchWrapper(original: Branching,
                                     boundaryManager:IntBoundaryManager,
                                     shouldClose:SynchronizedForceClose,
                                     cpobjective: CPObjectiveUnit) extends Branching {
  override def alternatives(): Seq[Alternative] = {
    original.alternatives().map((a: Alternative) => {
      () => {
        if(shouldClose.getShouldClose)
          throw new ClosedByForce
        cpobjective.updateWorstBound(boundaryManager.getBoundary())
        cpobjective.best = boundaryManager.getBoundary()
        a()
      }
    })
  }
}

/**
  * Wrapper for a solution manager.
  */
object CPIntBoundaryUpdateSolutionWrapper {
  def apply(original: Model => Unit, boundaryManager:IntBoundaryManager, variable: IntVar): Model => Unit = {
    (model: Model) => {
      val v = model.getRepresentative(variable)
      if(v.isBound)
        boundaryManager.updateBoundary(v.max)
      original(model)
    }
  }
}