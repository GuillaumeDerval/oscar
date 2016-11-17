package oscar.modeling.solvers.lp

import oscar.linprog.enums.{EndStatus, SolutionFound}
import oscar.modeling.models.lp.LPModel
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.modeling.solvers.Solve

class LPProgram[RetVal](md: ModelDeclaration = new ModelDeclaration()) extends Solve[RetVal] {
  implicit val program = this
  def solve(): (EndStatus, Option[RetVal]) = {
    val umodel = md.getCurrentModel.asInstanceOf[UninstantiatedModel]
    val model = LPModel.lpsolve(umodel)
    md.apply(model){
      val endStatus = model.solver.solve

      val realOnSolution = if(this.onSolution == null && md.isInstanceOf[Solve[RetVal]]) md.asInstanceOf[Solve[RetVal]].onSolution else onSolution
      val solution = if(endStatus == SolutionFound) Some(realOnSolution()) else None
      model.solver.release()

      (endStatus, solution)
    }
  }
}
