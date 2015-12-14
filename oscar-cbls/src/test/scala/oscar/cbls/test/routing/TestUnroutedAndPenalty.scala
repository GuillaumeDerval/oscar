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
/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by Ghilain Florent.
 * ****************************************************************************
 */

package oscar.cbls.test.routing

import scala.language.reflectiveCalls
import scala.math.pow
import scala.math.round
import scala.math.sqrt

import org.scalatest.{Matchers, FunSuite}
import org.scalatest.matchers.ShouldMatchers

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.routing.model.HopDistanceAsObjectiveTerm
import oscar.cbls.routing.model.PenaltyForUnrouted
import oscar.cbls.routing.model.PositionInRouteAndRouteNr
import oscar.cbls.routing.model.UnroutedImpl
import oscar.cbls.routing.model.VRP
import oscar.cbls.routing.model.VRPObjective

class TestUnroutedAndPenalty extends FunSuite with Matchers {

  def fixture =
    new {
      /**
       * Gives a distance matrix by entering the abscissa and
       * ordinates of points in the plan.
       */
      def getDistanceMatrix(abscissa: Array[Int], ordinate: Array[Int]): Array[Array[Int]] = {
        val N = abscissa.length
        Array.tabulate(N, N)((i, j) => round(sqrt((pow(abscissa(i) - abscissa(j), 2)
          + pow(ordinate(i) - ordinate(j), 2)).toFloat)).toInt)
      }
      val UNROUTED = 9
      val ROUTE_ARRAY_UNROUTED = 1
      val V: Int = 1
      val N: Int = 9

      val matrix = getDistanceMatrix(Array(0, 1, 2, 3, 4, 5, 6, 7, 8), Array(0, 0, 0, 0, 0, 0, 0, 0, 0))
      val model: Store = new Store(false, None, false, false)

      val vrp = new VRP(N, V, model) with UnroutedImpl with VRPObjective with HopDistanceAsObjectiveTerm with PositionInRouteAndRouteNr with PenaltyForUnrouted
      vrp.installCostMatrix(matrix)
      model.close()
      vrp.setCircuit(List(0, 1, 2, 3, 4, 5, 6, 7, 8))
      // 0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 (-> 0)
    }

  test("instantiation") {
    val f = fixture
    f.vrp.unroutedPenalty.value should be(0)
    f.vrp.weightUnroutedPenalty.foreach { v => v.value should be(0) }
  }

  test("fixe a penalty of 100 on node 1") {
    val f = fixture
    f.vrp.setUnroutedPenaltyWeight(100)
    f.vrp.weightUnroutedPenalty(1).value should be(100)
    f.vrp.unroutedPenalty.value should be(100 * f.vrp.unrouted.value.size)
  }

  test("fixe a penalty of 1000 on each node") {
    val f = fixture
    f.vrp.setUnroutedPenaltyWeight(1000)
    f.vrp.weightUnroutedPenalty.foreach { v => v.value should be(1000) }
    f.vrp.unroutedPenalty.value should be(0)
  }

  test("fixe a penalty of 100 on node 1 and unroute 1") {
    val f = fixture
    f.vrp.setUnroutedPenaltyWeight(1, 100)
    f.vrp.weightUnroutedPenalty(1).value should be(100)
    f.vrp.unroutedPenalty.value should be(0)

    f.vrp.unroute()

    f.vrp.unroutedPenalty.value should be(100)
  }
}
