package oscar.modeling.solvers.lp

import oscar.algebra.{Feasible, Linear, ModelStatus, Optimal, Solution}
import oscar.modeling.models.lp.LPModel
import oscar.modeling.models.{ModelDeclaration, ModelDeclarationProxy, UninstantiatedModel}
import oscar.modeling.solvers.{Solve, SolveHolder}

class LPProgram[RetVal](modelDeclaration: ModelDeclaration = new ModelDeclaration()) extends SolveHolder[RetVal](modelDeclaration) with ModelDeclarationProxy {
  implicit val program = this
  override implicit val md = modelDeclaration

  private def onSolverSolution(md: LPModel, realOnSolution: () => RetVal, solverSolution: Solution[Double]): RetVal = {
    md.setCurrentSolution(Some(solverSolution))
    val r = realOnSolution()
    md.setCurrentSolution(None)
    r
  }

  def solve(): (ModelStatus[Linear, Linear, Double], Option[RetVal]) = {
    val umodel = md.getCurrentModel.asInstanceOf[UninstantiatedModel]
    val model = new LPModel(umodel)
    md.apply(model){
      val solverRun = model.oscarLPModel.interface.run(model.oscarLPModel)
      val endStatus = solverRun.solve

      val realOnSolution = if(this.onSolution == null && md.isInstanceOf[Solve[RetVal]]) md.asInstanceOf[Solve[RetVal]].onSolution else onSolution
      val solution = endStatus match {
        case Optimal(sol) => Some(onSolverSolution(model, realOnSolution, sol))
        case Feasible(sol) => Some(onSolverSolution(model, realOnSolution, sol))
        case _ => None
      }

      solverRun.release()

      (endStatus, solution)
    }
  }
}