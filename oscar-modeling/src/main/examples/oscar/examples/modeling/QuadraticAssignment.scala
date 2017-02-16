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

import oscar.modeling.algebra.integer.Sum
import oscar.modeling.constraints.AllDifferent
import oscar.modeling.models.cp.CPModel
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.modeling.solvers.cp.{Branchings, CPApp}
import oscar.modeling.vars.IntVar
import oscar.util._

import scala.io.Source
import scala.spores._

/**
  * Example of QAP, copied from the original one from OscaR-lib.
  * GNU GPL, OscaR Authors
  */

object QuadraticAssignment extends CPApp[Int] with App {

  // Read the data
  var lines = Source.fromFile("qap.txt").getLines.toList.filter(_ != "")
  val n = lines.head.toInt
  val N = 0 until n
  lines = lines.drop(1)
  var w: Array[Array[Int]] = Array() //weight matrix
  var d: Array[Array[Int]] = Array() //distance matrix
  for (i <- N) {
    w = w :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
    lines = lines.drop(1)
  }
  for (i <- N) {
    d = d :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
    lines = lines.drop(1)
  }

  //onSolution { println("solution " + x.mkString(",")) }

  // for each facilities, the location chosen for it
  val x = N map (v => IntVar(0, n-1))

  add(AllDifferent(x.toArray))

  val obj = Sum(N, N)((i, j) => d(x(i))(x(j)) * w(i)(j)).reify()

  onSolutionF(spore {
    val x_ = obj
    () => {
      x_.max
    }
  })

  minimize(obj)

  val search = Branchings.fromAlternatives(spore{
        val _x = x
        (cp: CPModel) => {
          val z = _x.filter(y => !y.isBound)
          if(z.isEmpty)
            Branchings.noAlternative
          else {
            val vari = selectMinDeterministic(z)(y => y.size)
            val valu = vari.min
            Branchings.branch(cp.post(vari === valu))(cp.post(vari !== valu))
          }
        }
    }
  )

  setSearch(search)
  setDecompositionStrategy(new CartProdRefinement(x,search))

  val stats = solve()
  println(stats)
}