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
import oscar.algo.search._
import scala.io.Source
import scala.io.Source
import oscar.util._
import oscar.visual._

object ElementVarACPerf {
  
  def main(args: Array[String]): Unit = {
    val t = time {
      for (i <- 0 until 50) {
        println("seed:" + i)
        solve1(i)
      }
    }
    println("total time:" + t)
  }

  def solve1(seed: Int) = {
    val n = 51
    val prob = 30
    val rand = new scala.util.Random(seed)
    def randDom(maxVal: Int, prob: Int) = (0 to maxVal).filter(i => rand.nextInt(100) < prob)
    val cp = CPSolver()
    val y = Array.fill(n)(CPIntVar(cp, randDom(n, prob)))
    val x = CPIntVar(cp, randDom(n, prob * 2))
    val z = CPIntVar(cp, randDom(n, prob * 2))
    var nbsol = 0

    cp.add(elementVar(y, x, z), Strong)
    for (i <- 0 until y.size - 4 by 3; if rand.nextInt(100) < 30) {
      cp.add(y(i) + y(i + 1) == y(i + 2))
      cp.add(y(i) != y(i + 1))
    }
    cp.add(allDifferent(y.take(n / 10)), Medium)
    cp.search {
      binaryFirstFail(y ++ Array(x, z), _.min)
    } start (nSols = 1)
  }
}
