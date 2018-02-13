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

package oscar.examples.modeling

import oscar.modeling.algebra.integer.IntExpression
import oscar.modeling.constraints.AllDifferent
import oscar.modeling.solvers.SolverApp
import oscar.modeling.solvers.cp.{Branchings, CPSolving}
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.modeling.vars.IntVar

object GolombRuler extends SolverApp[String] with CPSolving {

  class GolombConfig extends AppConfig {
    val size = trailArg[Int](descr = "Size of the golomb ruler", name="size")
  }
  override lazy val config: GolombConfig = new GolombConfig


  def increasing(y: Array[IntVar]) = {
    for (i <- 1 until y.length) {
      post(y(i - 1) < y(i))
    }
  }

  // TODO @gderval fix this
  var n: Int = config.size()


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
  onSolution({
    val v = m.map(_.max).mkString(",")
    println(v)
    v
  })

  // Set the decomposition strategy, to be used in parallel CP
  setCPDecompositionStrategy(new CartProdRefinement(m, Branchings.binaryStatic(m)))

  println(solve())
}
