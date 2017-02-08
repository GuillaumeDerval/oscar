package oscar.modeling.solvers.mip

import de.xypron.linopt.SolverGlpk
import oscar.modeling.models.mip.MIPModel
import oscar.modeling.models.{ModelDeclaration, ModelDeclarationProxy, UninstantiatedModel}
import oscar.modeling.solvers.SolveHolder

class MIPProgram[RetVal](modelDeclaration: ModelDeclaration = new ModelDeclaration()) extends SolveHolder[RetVal] with ModelDeclarationProxy {
  implicit val program = this
  override implicit val md = modelDeclaration
  val solver = new SolverGlpk()

  def solve(): Option[RetVal] = {
    val umodel = md.getCurrentModel.asInstanceOf[UninstantiatedModel]
    val model = new MIPModel(umodel)
    md.apply(model){
      val out = solver.solve(model.linProblem)

      if(out) {
        model.hasSolution = true
        val out = Some(onSolution())
        model.hasSolution = false
        out
      }
      else
        None
    }
  }

  override protected val solveRedirectTo: Any = md
}