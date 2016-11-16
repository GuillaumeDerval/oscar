package oscar.modeling.examples

import oscar.modeling.constraints.AllDifferent
import oscar.modeling.models.UninstantiatedModel
import oscar.modeling.models.operators.SimplifySum
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.modeling.solvers.cp.{Branchings, CPApp, CPAppConfig}
import oscar.modeling.vars.IntVar
import oscar.modeling.visualisation.ConstraintsVisualisation

/**
  * Created by dervalguillaume on 2/06/16.
  */
object SendMoreMoney extends CPApp[Unit] with App {
  // variables
  val S = IntVar(0,9, "S")
  val E = IntVar(0,9, "E")
  val N = IntVar(0,9, "N")
  val D = IntVar(0,9, "D")
  val M = IntVar(0,9, "M")
  val O = IntVar(0,9, "O")
  val R = IntVar(0,9, "R")
  val Y = IntVar(0,9, "Y")
  val all = Array(S,E,N,D,M,O,R,Y)

  // constraints
  add(       S*1000 + E*100 + N*10 + D +
    M*1000 + O*100 + R*10 + E ===
    M*10000 + O*1000 + N*100 + E*10 + Y)
  //add(AllDifferent(all))

  val a = new ConstraintsVisualisation(SimplifySum(this.getCurrentModel.asInstanceOf[UninstantiatedModel]).constraints.toArray, "")
  a.display()
}