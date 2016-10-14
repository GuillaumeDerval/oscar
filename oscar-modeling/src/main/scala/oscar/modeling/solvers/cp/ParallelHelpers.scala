package oscar.modeling.solvers.cp

import oscar.cp.constraints.CPObjectiveUnit
import oscar.modeling.models.Model
import oscar.modeling.solvers.cp.branchings.Branching
import oscar.modeling.solvers.cp.branchings.Branching._
import oscar.modeling.vars.IntVar

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