package oscar.modeling.solvers

import org.rogach.scallop.Subcommand

/**
  * A Module for SolverApp. Represents a solver or a solving method.
  */
trait SolverAppModule {
  def name: String = subcommand.commandName

  /**
    * A Scallop subcommand containing all the needed information to start the solver, if needed
    */
  val subcommand: Subcommand

  /**
    * Solve the model
    */
  def solve[RetVal](): List[RetVal]

  /**
    * Called after the module has been selected, but before the (user-defined) constructor of the app
    */
  def onSelect(): Unit = {}
}
