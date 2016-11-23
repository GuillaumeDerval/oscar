package oscar.modeling.solvers.lp

import org.rogach.scallop.Subcommand
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.solvers.{SolveHolder, SolverApp, SolverAppModulable, SolverAppModule}

/**
  * A trait for SolverApp that indicates that the model is solvable using a sequential CP solver
  */
trait LPSolving extends SolverAppModulable {
  override def getModules: List[SolverAppModule] = new LPAppModule(this.app, this.md) :: super.getModules
}

/**
  * Module for SolverApp that solves models using a LP solver
  * @param app the SolverApp
  * @param modelDeclaration the ModelDeclaration linked to the SolverApp
  */
class LPAppModule(app: SolverApp[_], modelDeclaration: ModelDeclaration) extends SolverAppModule {
  class SequentialCPSubcommand extends Subcommand("lp") {
    descr("Solves the model using a LP solver.")
  }
  override val subcommand = new SequentialCPSubcommand

  override def solve[RetVal](): List[RetVal] = {
    val pg = new LPProgram[RetVal](modelDeclaration)
    val onSolution: () => RetVal = app.asInstanceOf[SolveHolder[RetVal]].onSolution
    if(onSolution == null)
      throw new RuntimeException("No onSolution defined in the SolverApp or in the ModelDeclaration")
    pg.onSolution{onSolution()}
    val result = pg.solve()
    result._2.toList
  }
}
