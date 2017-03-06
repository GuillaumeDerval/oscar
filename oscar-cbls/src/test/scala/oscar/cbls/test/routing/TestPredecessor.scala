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

import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.core.computation.Store
import oscar.cbls.business.routing.legacy.model._

class TestPredecessor extends FunSuite with Matchers {

  def fixture(n: Int, v: Int) =
    new {
      val UNROUTED = n
      val ROUTE_ARRAY_UNROUTED = v
      val V: Int = v
      val N: Int = n
      // model with check internal
      val model: Store = new Store(false, None, false, false)
      val vrp = new VRP(N, V, model) with VRPObjective with PositionInRouteAndRouteNr with UnroutedImpl with HopDistanceAsObjectiveTerm with Predecessors
      model.close()

      RandomInsert(vrp)
      model.propagate()
    }

  test("check internal of 50 points and 1 vehicle") {
    fixture(50, 1)
  }

  test("check internal of 50 points and 2 vehicle") {
    fixture(50, 2)
  }

  test("check internal of 50 points and 5 vehicle") {
    fixture(50, 5)
  }

  test("check internal of 100 points and 1 vehicle") {
    fixture(100, 1)
  }

  test("check internal of 100 points and 2 vehicle") {
    fixture(100, 2)
  }

  test("check internal of 100 points and 5 vehicle") {
    fixture(100, 5)
  }

  test("check internal of 500 points and 1 vehicle") {
    fixture(500, 1)
  }

  test("check internal of 500 points and 2 vehicle") {
    fixture(500, 2)
  }

  test("check internal of 500 points and 5 vehicle") {
    fixture(500, 5)
  }
}
