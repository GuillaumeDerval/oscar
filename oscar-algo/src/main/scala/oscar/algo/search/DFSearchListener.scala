package oscar.algo.search

/**
  * Created by saschavancauwelaert on 23/02/16.
  */
trait DFSearchListener {

  /** Called on Push events */
  def onPush(node : DFSearchNode) : Unit
  /**  Called on Pop events */
  def onPop(node : DFSearchNode) : Unit
  /** Called on branching */
  def onBranch(alternative : Alternative) : Unit
  /** Called when a failure occurs */
  def performFailureActions(): Unit
  /** Called when a solution is found */
  def performSolutionActions(): Unit
  /** Adds an action to execute when a failed node is found */
  def onFailure(action: => Unit): Unit
  /** Adds an action to execute when a solution node is found */
  def onSolution(action: => Unit): Unit
  /** Clear all actions executed when a solution node is found */
  def clearOnSolution(): Unit
  /** Clear all actions executed when a failed node is found */
  def clearOnFailure(): Unit

}

class DefaultDFSearchListener extends DFSearchListener {

  // Actions to execute in case of solution node
  private[this] var solutionActions : List[() => Unit] = null
  // Actions to execute in case of failed node
  private[this] var failureActions : List[() => Unit] = null

  def onPush(node : DFSearchNode) : Unit = {}
  def onPop(node : DFSearchNode) : Unit = {}
  def onBranch(alternative : Alternative) : Unit = {}

  final def onSolution(action: => Unit): Unit = solutionActions = onEvent(action, solutionActions)
  final def onFailure(action: => Unit): Unit = failureActions = onEvent(action, failureActions)

  final def clearOnSolution(): Unit = solutionActions = null
  final def clearOnFailure(): Unit = failureActions = null

  final def performSolutionActions() = perform(solutionActions)
  final def performFailureActions() = perform(failureActions)

  private final def onEvent(action: => Unit, actions : List[() => Unit]): List[() => Unit] = {
    if(actions == null)
      (() => action) :: Nil
    else
      (() => action) :: actions
  }

  private final def perform(actions : List[() => Unit]) = if(actions != null) actions.foreach(_())

}

object DefaultDFSearchListener {
  def apply() = new DefaultDFSearchListener()
}