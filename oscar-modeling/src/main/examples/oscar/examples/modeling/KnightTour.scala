package oscar.examples.modeling

import oscar.modeling.algebra.bool.{And, Or}
import oscar.modeling.algebra.integer.{Abs, IntExpression}
import oscar.modeling.constraints.AllDifferent
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.modeling.solvers.cp.{Branchings, CPApp}
import oscar.modeling.vars.IntVar

object KnightTour extends CPApp[String] with App {
  val x = Array.fill(36)(IntVar(0,36))
  post(AllDifferent(x))
  post(x(0) === 0)
  post(x(1) === 8)

  def dist(a: IntExpression, b: IntExpression): IntExpression = Abs(a-b)

  for(i <- 0 until 36) {
    val v1 = x(i)
    val v2 = x((i+1)%36)
    post(Or(And(dist(v1/6, v2/6) === 1, dist(v1%6, v2%6) === 2), And(dist(v1/6, v2/6) === 2, dist(v1%6, v2%6) === 1)))
  }

  setSearch(Branchings.binaryStatic(x))
  onSolution {
    x.zipWithIndex.map({case (v, idx) => "x["+idx+"]="+v.evaluate()}).mkString(" ")
  }

  setDecompositionStrategy(new CartProdRefinement(x, Branchings.binaryStatic(x)))
  val (stats, solutions) = solve(nSols = 1)
  println(solutions.mkString("\n"))
  println(stats)
}
