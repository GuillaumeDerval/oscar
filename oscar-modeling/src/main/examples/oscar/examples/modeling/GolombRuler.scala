package oscar.examples.modeling

import oscar.modeling.algebra.integer.IntExpression
import oscar.modeling.constraints.AllDifferent
import oscar.modeling.solvers.SolverApp
import oscar.modeling.solvers.cp.{Branchings, CPSolving, DistributedCPSolving}
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.modeling.vars.IntVar

import scala.spores._

object GolombRuler extends SolverApp[String] with CPSolving with DistributedCPSolving {
  override lazy val config = new AppConfig {
    val size = trailArg[Int](descr = "Size of the golomb ruler", name="size")
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
  post(m(n-1) < n*n)


  minimize(m(n - 1))

  // Set the search
  setCPSearch {Branchings.binaryStatic(m)}

  // The use of a spore allow to use the distributed module
  onSolutionF(spore {
    val m_ = m
    () => {
      val v = m_.map(_.max).mkString(",")
      println(v)
      v
    }
  })

  // Set the decomposition strategy, to be used in parallel CP
  setCPDecompositionStrategy(new CartProdRefinement(m, Branchings.binaryStatic(m)))

  println(solve())
}
