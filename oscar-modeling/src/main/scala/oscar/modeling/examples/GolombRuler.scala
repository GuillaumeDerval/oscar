package oscar.modeling.examples

import oscar.modeling.algebra.integer.IntExpression
import oscar.modeling.constraints.AllDifferent
import oscar.modeling.solvers.cp.decompositions.{CartProdRefinement, DecompositionAddCartProdInfo, DepthIterativeDeepening}
import oscar.modeling.solvers.cp.{Branchings, CPApp, CPAppConfig}
import oscar.modeling.vars.IntVar

import scala.spores._

/**
  * Example of ATSP, copied from the original one from OscaR-lib.
  * GNU GPL, OscaR Authors
  */
object GolombRuler extends CPApp[String] with App {
  override lazy val config = new CPAppConfig {
    val size = trailArg[Int](descr = "Size of the golomb ruler")
  }

  def increasing(y: Array[IntVar]) = {
    for (i <- 1 until y.length) {
      post(y(i - 1) < y(i))
    }
  }

  var n = config.size()

  val m = Array.fill(n)(IntVar(0,(1 << (n - 1))-1))

  post(m(0) === 0)

  increasing(m)

  // Number of marks and differences
  val n_d = (n*n-n)/2

  // Array of differences
  val d = Array.ofDim[IntExpression](n_d)

  var k = 0
  for(i <- 0 until n-1) {
    for(j <- i+1 until n) {
      d(k) = m(j)-m(i)
      post(d(k) >= ((j-i)*(j-i+1)/2))
      k += 1
    }
  }

  post(AllDifferent(d))

  if (n > 2)
    post(d(0) < d(n_d-1))

  minimize(m(n - 1))

  setSearch {
    Branchings.binaryStatic(m)
  }

  post(m(n-1) < n*n)

  onSolution(spore {
    val m_ = m
    () => {
      val v = m_.map(_.max).mkString(",")
      println(v)
      v
    }
  })

  //apply(SimplifySum)

  setDecompositionStrategy(new CartProdRefinement(m, Branchings.binaryStatic(m)))
  //setDecompositionStrategy(new DecompositionAddCartProdInfo(new DepthIterativeDeepening(Branching.naryStatic(m)), m))
  val (stats, solutions) = solve()
  println(stats)
  //println(solutions.last)
}
