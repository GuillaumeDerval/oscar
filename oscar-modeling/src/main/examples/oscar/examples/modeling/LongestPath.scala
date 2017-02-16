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

import oscar.modeling.algebra.integer.IntExpression._
import oscar.modeling.algebra.integer.Sum
import oscar.modeling.constraints.{AllDifferent, Table}
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.modeling.solvers.cp.{Branchings, CPApp}
import oscar.modeling.vars.IntVar

import scala.io.Source
import scala.spores._

/**
  * Example of LonguestPath, copied from the original one from OscaR-lib.
  * GNU GPL, OscaR Authors
  */
object LongestPath extends CPApp[Int] with App {

  // --- reading the data ---

  val lines = Source.fromFile("planar-n200.ins1.txt_com10_ins1").getLines.reduceLeft(_ + " " + _)

  val vals = lines.split("[ ,\t]").toList.filterNot(_ == "")
  var index = 0
  def next() = {
    index += 1
    vals(index - 1)
  }
  val nbNodes = next().toInt // last one is a dummy start/end to use circuit
  val nbArcs = next().toInt
  val nodes = 0 until nbNodes
  println("nbNodes:" + nbNodes + " nbArcs:" + nbArcs)
  val absent = -10000000
  val distMatrix = Array.fill(nbNodes, nbNodes)(absent)
  for (i <- 1 to nbArcs) {
    val from = next().toInt - 1
    val to = next().toInt - 1
    val w: Int = (next().toDouble * 100).toInt
    distMatrix(from)(to) = w
    distMatrix(to)(from) = w
  }

  def successors(i: Int): Set[Int] = nodes.filter(distMatrix(i)(_) != absent).toSet

  val outNodes = Array.tabulate(nbNodes)(i => nodes.filter(distMatrix(i)(_) != absent).toSet)
  val inNodes = Array.tabulate(nbNodes)(i => nodes.filter(distMatrix(i)(_) != absent).toSet)

  // set of valid transitions pair
  val tuples = (for (i <- nodes; j <- successors(i)) yield (i, j)).toSet

  val distMatrix_ : Array[Array[Int]] = Array.tabulate(nbNodes, nbNodes)((i, j) => if (distMatrix(i)(j) == absent) 0 else distMatrix(i)(j))

  val len = 12 // path lenth

  println("----------- trying with length:" + len + "-------------")

  val path: Array[IntVar] = Array.fill(len)(IntVar(0, nbNodes))

  val weight = Sum(0 until len - 1)(i => distMatrix_(path(i))(path(i + 1))).reify()

  for (i <- 0 until len - 1) {
    add(Table(path(i), path(i + 1), tuples)) // for each consecutive visits, give the possible valid transitions
  }
  post(AllDifferent(path))

  maximize(weight)
  setSearch(Branchings.binaryFirstFail(path))
  setDecompositionStrategy(new CartProdRefinement(path, Branchings.binaryFirstFail(path)))

  onSolutionF(spore {
    val x_ = weight
    () => {
      x_.max
    }
  })

  val stats = solve()
  println(stats)
}