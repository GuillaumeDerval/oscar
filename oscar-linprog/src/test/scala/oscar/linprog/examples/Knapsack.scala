/** *****************************************************************************
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
  * *****************************************************************************/

package oscar.linprog.examples

import oscar.algebra._
import oscar.linprog.MPModel
import oscar.linprog.lpsolve.LPSolve

/*
 * The knapsack problem is a well-known problem in combinatorial optimization:
 * Given a set of items, each with a weight and an utility, determine the count of each item
 * to include in a collection so that the total weight is less than or equal to a given limit
 * and the total utility is as large as possible.
 *
 * @author gme
 */
object Knapsack extends MPModel(LPSolve) with App {

  case class O(weight: Int, utility: Int, x: Var[Double])

  val weights = Array(100, 50, 45, 20, 10, 5)
  val utility = Array(40, 35, 18, 4, 10, 2)

  val objects = Array.tabulate(weights.length)(i => O(weights(i), utility(i), MPIntVar(s"x_$i", 0 to 1)))

  val capacity = 100

  // maximize total utility
  maximize(sum(objects)(o => o.x * o.utility.toDouble))

  // given the limited capacity of the pack
  add(s"C_1" |: sum(objects)(o => o.x * o.weight.toDouble) <= capacity.toDouble)

  solve match {
    case Optimal(solution) =>

      val selected = objects.filter(o => solution(o.x) >= .9)
      var totalWeight = selected.map(o => o.weight).sum

      println("Total Utility: " + solution(objective.expression))
      println("Total Weight: " + totalWeight)

  }
}
