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

import org.scalatest.prop.Checkers
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.routing.model.{PositionInRouteAndRouteNr, RoutedAndUnrouted, VRP, VRPObjective}
import oscar.cbls.test.routing.TestBench.{onePoint, swap, threeOpt, twoOpt}

/**
 * The tests marked with a star (*) require the assertion mechanism of IntVar in ComputationStructure file, which
 * verifies the domain of a variable variable.
 * These tests (with star) show the lack of robustness of the current framework.
 */
class TestMove extends FunSuite with Matchers with Checkers {
  moveTest("The first improving one point move is done correctly.", 1, true) {
    onePoint()
  }

  moveTest("The best improving one point move is done correctly.", 1, true) {
    onePoint(best = true)
  }

  moveTest("The first improving swap is done correctly.", 1, true) {
    swap()
  }

  moveTest("The best improving swap is done correctly.", 1, true) {
    swap(best = true)
  }

  moveTest("A basic two-opt move is done correctly.", 1, false, 4, 1,
    Array(0, 0, 2, 2), Array(0, 2, 0, 2),
    (vrp: VRP with VRPObjective with PositionInRouteAndRouteNr) => {
      vrp.setCircuit(List(0, 1, 2, 3))
    }) {
      twoOpt()
    }

  moveTest("A first two-opt move is done correctly.", 1, true) {
    twoOpt()
  }

  moveTest("A best two-opt move is done correctly.", 1, true) {
    twoOpt(best = true)
  }

  moveTest("A first three-opt move is done correctly.", verbose = 1, randomWeight = true) {
    threeOpt()
  }

  moveTest("A best three-opt move is done correctly.", verbose = 1, randomWeight = true) {
    threeOpt(best = true)
  }

  moveTest("A first three-optKK move is done correctly.", 1, true) {
    threeOpt(kk = true)
  }

  moveTest("A best three-optKK move is done correctly.", 1, true) {
    threeOpt(best = true, kk = true)
  }

  def moveTest(
    name: String,
    verbose: Int = 0,
    randomWeight: Boolean = false,
    nbNodes: Int = 10,
    nbVehicles: Int = 1,
    abscissa: Array[Int] = null,
    ordinate: Array[Int] = null,
    init: VRP with RoutedAndUnrouted with VRPObjective with PositionInRouteAndRouteNr => Unit = RandomInsert.apply)(moveFun: MoveFixture => Boolean): Unit = {
    test(name) {
      var improvingMoveFound = false
      while (!improvingMoveFound) {
        val f = new MoveFixture(verbose, randomWeight, nbNodes, nbVehicles, abscissa, ordinate, init)

        if (verbose > 1) {
          println(f.vrp)
        }
        improvingMoveFound = moveFun(f)
        if (verbose > 0 && !improvingMoveFound) {
          println("No improving move found for the following problem:")
          println(f.model)
          println(f.vrp)
        }
      }
    }
  }
}
