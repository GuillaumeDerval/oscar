/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.modeling.experiences

import oscar.modeling.constraints.AllDifferent
import oscar.modeling.misc.CartesianProduct
import oscar.modeling.models.cp.MemoCPModel
import oscar.modeling.models.{NoSolException, UninstantiatedModel}
import oscar.modeling.solvers.cp.decompositions.{DecompositionAddCartProdInfo, DepthIterativeDeepening}
import oscar.modeling.solvers.cp.distributed.SubProblemCartesianProductLog
import oscar.modeling.solvers.cp.{Branchings, CPApp, CPAppConfig}
import oscar.modeling.vars.IntVar

import scala.language.reflectiveCalls

object CartProdExperienceGolombRuler extends CPApp[String] with App {
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
  val decompose = new DecompositionAddCartProdInfo(new DepthIterativeDeepening(Branchings.binaryStatic(m)), m)
  val subproblems = decompose.decompose(this.md.getCurrentModel.asInstanceOf[UninstantiatedModel], 1952)
  //val decompose = new CartProdRefinement(m, Branching.naryStatic(m))
  //val subproblems = decompose.decompose(this.modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel], 2000)
  //val decompose = new CartProdRefinement(m, Branching.binaryStatic(m))
  //val subproblems = decompose.decompose(this.modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel], 2000)

  val memo = new MemoCPModel(this.md.getCurrentModel.asInstanceOf[UninstantiatedModel])
  var bestBound = m(n-1).max
  memo.cpSolver.silent = true
  memo.cpSolver.onSolution({bestBound = m(n-1).max})
  memo.apply {
    memo.cpSolver.search(Branchings.binaryStatic(m)(memo))
    for((sp, idx) <- subproblems.zipWithIndex) {
      memo.pushState()
      val ok = try {
        sp.constraints.foreach(c => memo.post(c))
        true
      }
      catch {
        case c: NoSolException => false
      }


      val original_cp = sp.getData(SubProblemCartesianProductLog).get
      val current_cp = if(ok) CartesianProduct.computeLog(m) else 0
      val bound_before = bestBound
      println(s"$idx\t$original_cp\t$current_cp\t$bound_before")
      memo.cpSolver.start()
      memo.popState()
    }
  }
}