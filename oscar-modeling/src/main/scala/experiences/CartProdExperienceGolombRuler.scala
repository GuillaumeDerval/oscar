package experiences

import constraints.AllDifferent
import examples.GolombRuler._
import misc.CartesianProduct
import models.{MemoCPModel, UninstantiatedModel}
import solvers.cp.branchings.Branching
import solvers.cp.decompositions.{CartProdRefinement, DecompositionAddCartProdInfo, DepthIterativeDeepening}
import solvers.cp.{DistributedCPApp, DistributedCPAppConfig, SubProblemCartesianProductLog}
import vars.IntVar

import scala.spores._

object CartProdExperienceGolombRuler extends DistributedCPApp[String] with App {
  override lazy val config = new DistributedCPAppConfig {
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
  val d = Array.ofDim[IntVar](n_d)

  var k = 0
  for(i <- 0 until n-1) {
    for(j <- i+1 until n) {
      d(k) = (m(j)-m(i)).reify()
      post(d(k) >= ((j-i)*(j-i+1)/2))
      k += 1
    }
  }

  post(AllDifferent(d))

  if (n > 2)
    post(d(0) < d(n_d-1))

  minimize(m(n - 1))

  post(m(n-1) < n*n)

  //val decompose = new DecompositionAddCartProdInfo(new DepthIterativeDeepening(Branching.naryStatic(m)), m)
  //val subproblems = decompose.decompose(this.modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel], 1952)
  val decompose = new DecompositionAddCartProdInfo(new DepthIterativeDeepening(Branching.binaryStatic(m)), m)
  val subproblems = decompose.decompose(this.modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel], 1952)
  //val decompose = new CartProdRefinement(m, Branching.naryStatic(m))
  //val subproblems = decompose.decompose(this.modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel], 2000)
  //val decompose = new CartProdRefinement(m, Branching.binaryStatic(m))
  //val subproblems = decompose.decompose(this.modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel], 2000)

  val memo = new MemoCPModel(this.modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel])
  var bestBound = m(n-1).max
  memo.cpSolver.silent = true
  memo.cpSolver.onSolution({bestBound = m(n-1).max})
  memo.cpSolver.search(Branching.binaryStatic(m)(memo))
  memo.apply {
    for((sp, idx) <- subproblems.zipWithIndex) {
      memo.pushState()
      val ok = sp.constraints.forall(c => memo.post(c))

      val original_cp = sp.getData(SubProblemCartesianProductLog).get
      val current_cp = if(ok) CartesianProduct.computeLog(m) else 0
      val bound_before = bestBound
      println(s"$idx\t$original_cp\t$current_cp\t$bound_before")
      memo.cpSolver.start()
      memo.popState()
    }
  }
}