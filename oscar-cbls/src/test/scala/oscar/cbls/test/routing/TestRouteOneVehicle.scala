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

import org.scalatest.{Matchers, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import oscar.cbls.invariants.core.computation.{ CBLSIntVar, Store }
import oscar.cbls.invariants.lib.logic.Routes
import scala.language.reflectiveCalls

import oscar.cbls.modeling.Algebra._
/**
 * The tests marked with a star (*) require the assertion mechanism of IntVar in ComputationStructure file, which
 * verifies the domain of a variable variable.
 * These tests (with star) show the lack of robustness of the current framework.
 */
class TestRouteOneVehicle extends FunSuite with Matchers {

  def fixture =
    new {
      val UNROUTED = 6
      val ROUTE_ARRAY_UNROUTED = 1
      val nbPoints = 6
      val nbCars = 1
      val model = new Store(false, None, false, false)
      val next = Array.tabulate(nbPoints)(i =>
        if (i < nbCars) CBLSIntVar(model, i, i to nbPoints - 1, "next" + i)
        else CBLSIntVar(model, i, 0 to nbPoints, "next" + i))
      // 0->1->2->3->4->5(->0)
      next(0) := 1
      next(1) := 2
      next(2) := 3
      next(3) := 4
      next(4) := 5
      next(5) := 0
      val routes = Routes.buildRoutes(next, nbCars)
      model.close()
    }

  test("Instantiation") {
    val f = fixture

    f.routes.routeLength(0).value should be(6)
    f.routes.lastInRoute(0).value should be(5)
    for (i <- 0 to 5)
      f.routes.positionInRoute(i).value should be(i)
  }

  test("Unroute 4") {
    val f = fixture
    f.next(3) := 5
    f.next(4) := f.UNROUTED

    f.routes.routeLength(0).value should be(5)
    f.routes.lastInRoute(0).value should be(5)
    for (i <- 0 to 5) {
      if (i <= 3)
        f.routes.positionInRoute(i).value should be(i)
      else if (i == 4) {
        f.routes.positionInRoute(i).value should be(f.UNROUTED)
        f.routes.routeNr(i).value should be(f.ROUTE_ARRAY_UNROUTED)
      } else
        f.routes.positionInRoute(i).value should be(4)
    }
  }

  test("* Unroute depot") {
    val f = fixture
    f.model.propagate()

    an [java.lang.Error] should be thrownBy {
      f.next(5) := 1
      f.next(0) := f.UNROUTED
      f.model.propagate()
    }
  }

  test("Unroute 2 and 4") {
    val f = fixture
    f.next(1) := 3
    f.next(2) := f.UNROUTED
    f.next(3) := 5
    f.next(4) := f.UNROUTED

    f.routes.routeLength(0).value should be(4)
    f.routes.lastInRoute(0).value should be(5)
    for (i <- 0 to 5) {
      if (i <= 1)
        f.routes.positionInRoute(i).value should be(i)
      else if (i == 2) {
        f.routes.positionInRoute(i).value should be(f.UNROUTED)
        f.routes.routeNr(i).value should be(f.ROUTE_ARRAY_UNROUTED)
      } else if (i == 3)
        f.routes.positionInRoute(i).value should be(2)
      else if (i == 4) {
        f.routes.positionInRoute(i).value should be(f.UNROUTED)
        f.routes.routeNr(i).value should be(f.ROUTE_ARRAY_UNROUTED)
      } else
        f.routes.positionInRoute(i).value should be(3)
    }
  }

  test("move 1 after 5") {
    val f = fixture
    f.next(5) := 1
    f.next(1) := 0
    f.next(0) := 2

    f.routes.routeLength(0).value should be(6)
    f.routes.lastInRoute(0).value should be(1)
    for (i <- 0 to 5) {
      if (i == 0)
        f.routes.positionInRoute(i).value should be(0)
      else if (i == 1)
        f.routes.positionInRoute(i).value should be(5)
      else
        f.routes.positionInRoute(i).value should be(i - 1)
    }
  }

  test("swap 1-2 with 4-5") {
    val f = fixture
    f.next(0) := 4
    f.next(5) := 3
    f.next(3) := 1
    f.next(2) := 0

    f.routes.routeLength(0).value should be(6)
    f.routes.lastInRoute(0).value should be(2)
    for (i <- 0 to 5) {
      if (i == 0)
        f.routes.positionInRoute(i).value should be(0)
      else if (i == 1)
        f.routes.positionInRoute(i).value should be(4)
      else if (i == 2)
        f.routes.positionInRoute(i).value should be(5)
      else if (i == 3)
        f.routes.positionInRoute(i).value should be(3)
      else if (i == 4)
        f.routes.positionInRoute(i).value should be(1)
      else if (i == 5)
        f.routes.positionInRoute(i).value should be(2)
    }
  }

  test("unroute 4 and 5, then add 5 after 1") {
    val f = fixture
    f.next(4) := f.UNROUTED
    f.next(5) := f.UNROUTED
    f.next(3) := 0
    f.next(1) := 5
    f.next(5) := 2

    f.routes.routeLength(0).value should be(5)
    f.routes.lastInRoute(0).value should be(3)
    for (i <- 0 to 5) {
      if (i <= 1)
        f.routes.positionInRoute(i).value should be(i)
      else if (i == 5)
        f.routes.positionInRoute(i).value should be(2)
      else if (i == 4) {
        f.routes.positionInRoute(i).value should be(f.UNROUTED)
        f.routes.routeNr(i).value should be(f.ROUTE_ARRAY_UNROUTED)
      } else
        f.routes.positionInRoute(i).value should be(i + 1)
    }
  }

  test("* form a cycle") {
    val f = fixture

    an [java.lang.Error] should be thrownBy {
      f.next(4) := 3
      f.model.propagate()
    }

  }

  test("form a temp cycle") {
    val f = fixture

    f.next(4) := 3
    f.next(3) := 5
    f.next(2) := 4

    f.routes.routeLength(0).value should be(6)
    f.routes.lastInRoute(0).value should be(5)
    for (i <- 0 to 5) {
      if (i <= 2 || i == 5)
        f.routes.positionInRoute(i).value should be(i)
      else if (i == 3)
        f.routes.positionInRoute(i).value should be(4)
      else if (i == 4)
        f.routes.positionInRoute(i).value should be(3)
    }
  }

  test("* temp depot unrouted") {
    val f = fixture

    an [java.lang.Error] should be thrownBy {
      f.next(0) := f.UNROUTED
      f.next(4) := 1
      f.next(5) := 2
      f.next(0) := 5
      f.model.propagate()
    }
  }
}
