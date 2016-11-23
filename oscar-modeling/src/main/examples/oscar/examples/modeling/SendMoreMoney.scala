package oscar.examples.modeling

import oscar.modeling.constraints.AllDifferent
import oscar.modeling.solvers.SolverApp
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.modeling.solvers.cp.{Branchings, CPSolving}
import oscar.modeling.vars.IntVar

/**
  * Created by dervalguillaume on 23/11/16.
  */
object SendMoreMoney extends SolverApp[String] with CPSolving {
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
  add(S*1000 + E*100 + N*10 + D +
      M*1000 + O*100 + R*10 + E ===
      M*10000 + O*1000 + N*100 + E*10 + Y)
  add(AllDifferent(all))
  add(S !== 0)
  add(M !== 0)

  setCPSearch(Branchings.binaryFirstFail(all))
  setCPDecompositionStrategy(new CartProdRefinement(all, Branchings.binaryFirstFail(all)))
  onSolution(all.map(_.min.toString).mkString("-"))
  println(solve())
}
