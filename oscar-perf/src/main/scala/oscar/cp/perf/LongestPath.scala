package oscar.cp.perf

/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

import oscar.cp._

import scala.io.Source

/**
 * Longest Path Problem
 * @author Pierre Schaus pschaus@gmail.com
 */
object LongestPath extends CPModel with App {

  // Read the data
  val lines = Source.fromFile("data/longestpath/planar-n50.ins1.txt_com10_ins1").getLines.reduceLeft(_ + " " + _)
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

  val distMatrix_ = Array.tabulate(nbNodes, nbNodes)((i, j) => if (distMatrix(i)(j) == absent) 0 else distMatrix(i)(j))

  val len = 12 // path lenth

  solver.silent = true
  val path = Array.fill(len)(CPIntVar(nodes))
  val weight = sum(0 until len - 1)(i => distMatrix_(path(i))(path(i + 1)))

  maximize(weight)

  for (i <- 0 until len - 1) {
    add(table(path(i), path(i + 1), tuples)) // for each consecutive visits, give the possible valid transitions
  }

  add(allDifferent(path), Weak)

  search { binaryFirstFail(path) }

  val stats = start()
  println(stats)
}
