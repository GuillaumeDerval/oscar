package oscar.modeling.solvers.cp

import org.rogach.scallop.Subcommand
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.modeling.solvers.cp.Branchings.BranchingInstantiator
import oscar.modeling.solvers.{SolveHolder, SolverApp, SolverAppModulable, SolverAppModule}

/**
  * A trait for SolverApp that indicates that the model is solvable using a sequential CP solver
  */
trait SequentialCPSolving extends SolverAppModulable with CPSearchHolder {
  override def getModules: List[SolverAppModule] = new SequentialCPAppModule(this.app, this.md) :: super.getModules
}

/**
  * Module for SolverApp that solves models using a sequential CP solver
  * @param app the SolverApp
  * @param modelDeclaration the ModelDeclaration linked to the SolverApp
  */
class SequentialCPAppModule(app: SolverApp[_], modelDeclaration: ModelDeclaration) extends SolverAppModule {
  class SequentialCPSubcommand extends Subcommand("cp") {
    descr("Solves the model using a CP solver.")
    val timeout = opt[Int](name="timeout", short='t', descr = "Timeout for the *solving*, in milliseconds. 0 (default) means no timeout", default = Some(0))
    val nSols = opt[Int](name="nsols", short='n',  descr = "Maximum number of solutions to find before stopping the solve. 0 (default) tells the solver to find all the solutions", default = Some(0))
  }
  override val subcommand = new SequentialCPSubcommand

  override def solve[RetVal](): List[RetVal] = {
    val pg = new CPProgram[RetVal](modelDeclaration)
    val onSolution: () => RetVal = app.asInstanceOf[SolveHolder[RetVal]].onSolution
    if(onSolution == null)
      throw new RuntimeException("No onSolution defined in the SolverApp or in the ModelDeclaration")
    val search: BranchingInstantiator = app.asInstanceOf[CPSearchHolder].getCPSearch
    if(search == null)
      throw new RuntimeException("No search defined in the SolverApp or in the ModelDeclaration")
    val nSols = subcommand.nSols()
    val time = subcommand.timeout()
    pg.onSolution{onSolution()}
    pg.setSearch(search)
    val result = pg.solveLocally(modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel], nSols, time)
    result._2
  }
}
