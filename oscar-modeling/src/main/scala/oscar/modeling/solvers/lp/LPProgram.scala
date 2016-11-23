package oscar.modeling.solvers.lp

import oscar.linprog.enums.EndStatus
import oscar.modeling.models.lp.LPModel
import oscar.modeling.models.{ModelDeclaration, ModelDeclarationProxy, UninstantiatedModel}
import oscar.modeling.solvers.lp.LPProgram.{withLPSolve, withWhichSolver}
import oscar.modeling.solvers.{Solve, SolveHolder}

class LPProgram[RetVal](modelDeclaration: ModelDeclaration = new ModelDeclaration()) extends SolveHolder[RetVal](modelDeclaration) with ModelDeclarationProxy {
  implicit val program = this
  override implicit val md = modelDeclaration

  def solve(withSolver: withWhichSolver = withLPSolve()): (EndStatus, Option[RetVal]) = {
    val umodel = md.getCurrentModel.asInstanceOf[UninstantiatedModel]
    val model = withSolver match {
      case withLPSolve => LPModel.lpsolve(umodel)
      case withGurobi => LPModel.gurobi(umodel)
    }
    md.apply(model){
      val endStatus = model.solver.solve

      val realOnSolution = if(this.onSolution == null && md.isInstanceOf[Solve[RetVal]]) md.asInstanceOf[Solve[RetVal]].onSolution else onSolution
      val solution = if(model.solver.hasSolution) Some(realOnSolution()) else None
      model.solver.release()

      (endStatus, solution)
    }
  }
}

object LPProgram {
  abstract class withWhichSolver
  case class withLPSolve() extends withWhichSolver
  case class withGurobi() extends withWhichSolver
}