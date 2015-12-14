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

package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._

import oscar.cp._

class TestTableAC5TCRecomp extends FunSuite with ShouldMatchers {

  test("TableAC5 Test 1") {
    val cp = CPSolver()
    var x = Array.fill(3)(CPIntVar(1 to 3)(cp))

    val tuples = Array((1, 1, 1),
      (1, 2, 3))

    cp.post(new TableAC5TCRecomp(x(0), x(1), x(2), tuples))

    x(0).isBound should be(true)
    x(0).value should be(1)
    x(2).hasValue(2) should be(false)

    //println(x.mkString(","))

    cp.post(x(2) != 3)
    //println(x.mkString(","))

    cp.isFailed should be(false)
    x(1).value should be(1)
    x(2).value should be(1)

  }

  test("TableAC5 Test 2") {
    val cp = CPSolver()

    var x = CPIntVar(0 to 4)(cp)
    var y = CPIntVar(0 to 4)(cp)
    var z = CPIntVar(0 to 24)(cp)

    val tuples = (for (i <- 0 until 5; j <- i + 1 until 5) yield (i, j, i * 4 + j - 1)).toArray
    cp.post(new TableAC5TCRecomp(x, y, z, tuples))
    cp.post(z == 0)
    x.value should be(0)
    y.value should be(1)
    z.value should be(0)

  }

  test("TableAC5 Test 3") {
    val cp = CPSolver()
    var x = Array.fill(3)(CPIntVar(1 to 7)(cp))
    val tuples = Array((1, 1, 1), (1, 2, 3), (1, 2, 7), (2, 1, 4))
    var nbSol = 0
    cp.add(new TableAC5TCRecomp(x(0), x(1), x(2), tuples))
    cp.search {
      binaryStatic(x)
    } onSolution {
      nbSol += 1
    } start ()
    nbSol should be(4)

  }

  test("TableAC5 Test 4") {
    val cp = CPSolver()
    var x = Array.fill(2)(CPIntVar(1 to 1)(cp))

    val tuples = Array((1, 2), (2, 1))

    cp.post(new TableAC5TCRecomp(x(0), x(1), tuples))
    cp.isFailed should be(true)

  }

  test("TableAC5 Test 5") {

    def nbSol(newcons: Boolean) = {
      val cp = CPSolver()
      val x = Array.fill(4)(CPIntVar(Set(1, 3, 6, 9))(cp))

      val tuples = Array((1, 2, 2, 4),
        (1, 2, 4, 8),
        (1, 1, 9, 6),
        (1, 1, 8, 6),
        (3, 1, 6, 9),
        (1, 9, 3, 1),
        (1, 9, 9, 9),
        (3, 6, 6, 6))

      val cons = if (newcons) new TableAC5TCRecomp(x(0), x(1), x(2), x(3), tuples) else new TableSTR2(x, tuples.map(_.productIterator.asInstanceOf[Iterator[Int]].toArray))
      cp.post(cons)
      var nbSol = 0
      cp.search {
        binaryFirstFail(x)
      } onSolution {
        nbSol += 1
      } start ()
      nbSol
    }
    nbSol(false) should be(nbSol(true))

  }

}
