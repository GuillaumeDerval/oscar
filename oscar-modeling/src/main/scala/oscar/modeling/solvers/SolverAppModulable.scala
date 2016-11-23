package oscar.modeling.solvers

import oscar.modeling.models.ModelDeclaration

/**
  * Trait that adds a "module" (a type of solver) to a SolverApp
  */
trait SolverAppModulable {
  val md: ModelDeclaration
  val app: SolverApp[_]

  /**
    * Returns a list of module. For all modules, should be in the form
    * myModule :: super.getModules()
    */
  def getModules: List[SolverAppModule] = Nil
}
