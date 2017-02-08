package oscar.modeling.solvers.mip

import oscar.modeling.solvers.{SolverAppModulable, SolverAppModule}

/**
  * A trait for SolverApp that indicates that the model is solvable using a sequential CP solver
  */
trait MIPSolving extends SolverAppModulable {
  override def getModules: List[SolverAppModule] = new MIPAppModule(this.app) :: super.getModules
}
