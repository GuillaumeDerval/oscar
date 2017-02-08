package oscar.modeling.solvers.cp

import org.rogach.scallop.Subcommand
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.solvers.cp.Branchings.BranchingInstantiator
import oscar.modeling.solvers.cp.decompositions.DecompositionStrategy
import oscar.modeling.solvers.{SolveHolder, SolverApp, SolverAppModulable, SolverAppModule}

/**
  * A trait for SolverApp that indicates that the model is solvable using a parallel CP solver
  */
trait ParallelCPSolving extends SolverAppModulable with CPSearchHolder with CPDecompositionHolder {
  override def getModules: List[SolverAppModule] = new ParallelCPAppModule(this.app) :: super.getModules
}

/**
  * Module for SolverApp that solves models using a parallel CP solver (using EPS)
  * @param app the SolverApp
  */
class ParallelCPAppModule(app: SolverApp[_]) extends SolverAppModule {
  class ParallelCPSubcommand extends Subcommand("parallel-cp") {
    descr("Solves the model using a parallel CP solver.")
    val timeout = opt[Int](name="timeout", short='t', descr = "Timeout for the *solving*, in milliseconds. 0 (default) means no timeout", default = Some(0))
    val nSols = opt[Int](name="nsols", short='n',  descr = "Maximum number of solutions to find before stopping the solve. 0 (default) tells the solver to find all the solutions", default = Some(0))
    val sppw = opt[Int](name="sppw", short='s', descr = "Number of subproblems per thread. Default is 100.", default = Some(100))
    val threads = opt[Int](name="cpus", short='c', descr = "Number of threads to use. Default to the number of CPU.", default = Some(Runtime.getRuntime.availableProcessors()))
  }
  override val subcommand = new ParallelCPSubcommand

  override def solve[RetVal](): List[RetVal] = {
    val pg = new CPProgram[RetVal](app.modelDeclaration)
    val onSolution: () => RetVal = app.asInstanceOf[SolveHolder[RetVal]].onSolution
    if(onSolution == null)
      throw new RuntimeException("No onSolution defined in the SolverApp or in the ModelDeclaration")
    val search: BranchingInstantiator = app.asInstanceOf[CPSearchHolder].getCPSearch
    if(search == null)
      throw new RuntimeException("No search defined in the SolverApp or in the ModelDeclaration")
    var decompose: DecompositionStrategy = app.asInstanceOf[CPDecompositionHolder].getCPDecompositionStrategy
    if(decompose == null)
      throw new RuntimeException("No decomposition defined in the SolverApp or in the ModelDeclaration")
    val nSols = subcommand.nSols()
    val time = subcommand.timeout()
    pg.onSolution{onSolution()}
    pg.setSearch(search)
    pg.setDecompositionStrategy(decompose)
    val result = pg.solveParallel(subcommand.threads(), subcommand.sppw(), nSols, time)
    result._2
  }
}
