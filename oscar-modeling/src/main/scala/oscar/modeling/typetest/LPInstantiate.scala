package oscar.modeling.typetest

import oscar.linprog.interface.lpsolve.{LPSolve, LPSolveLib}
import oscar.linprog.modeling._

// To be adapted to actually return an oscar.modeling.models.InstantiatedModel
// This is just a test / proof of concept
object LPInstantiate {
  def apply(model: ParametrizedLinearModel) = {
    val solver = new MPSolver(LPSolveLib.createSolver).solverInterface
    val ids = new Array[Int](model.x.length)
    for (v <- model.x) {
      v.id = solver.addVariable(v.name, v.lb, v.ub)
      v.accessor = solver.solution(v.id)
    }
    for (i <- model.values.indices) {
      solver.addConstraint("truc", model.coeffs(i).map(_.evaluate().asDouble), model.vars(i).map(_.id), "==", model.values(i).evaluate().asDouble)
    }
    solver.solve
  }
}
