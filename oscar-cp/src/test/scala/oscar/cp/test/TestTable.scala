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
import oscar.cp.constraints.tables.TableAlgo
import oscar.cp.constraints.tables.table
import oscar.cp.constraints.tables.TableDecomp
import oscar.cp.testUtils._
import oscar.cp._

/**
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class TestTable extends TestSuite {

  private val rand = new scala.util.Random(16)

  private def randomTuples(dim: Int, n: Int, minValue: Int, maxValue: Int) = {
    Array.fill(n, dim)(rand.nextInt(maxValue - minValue) + minValue)
  }
  
  // Create the unit tests
  for (i <- 1 to 1000) {
    
    val tuples1 = randomTuples(3, 100, 3, 8)
    val tuples2 = randomTuples(3, 100, 2, 7)
    val tuples3 = randomTuples(3, 100, 1, 6)
    
    for (algo <- TableAlgo.values) {
      test("Test random tables " + i + " (" + algo.toString + ")") {
        testTable(Array(tuples1, tuples2, tuples3), algo)
      }
    }
  }

  private def testTable(tables: Array[Array[Array[Int]]], algo: TableAlgo.Value): Unit = {

    implicit val solver = CPSolver()
    val x = Array.fill(5)(CPIntVar(1 to 8))

    solver.add(allDifferent(x))
    solver.search(binaryFirstFail(x, _.max))

    val statRef = solver.startSubjectTo() {
      val cons = Seq(
        new TableDecomp(Array(x(0), x(1), x(2)), tables(0)),
        new TableDecomp(Array(x(2), x(3), x(4)), tables(1)),
        new TableDecomp(Array(x(0), x(2), x(4)), tables(2))
      )
      solver.add(cons)
    }

    val stat = solver.startSubjectTo() {
      val cons = Seq(
        table(Array(x(0), x(1), x(2)), tables(0), algo),
        table(Array(x(2), x(3), x(4)), tables(1), algo),
        table(Array(x(0), x(2), x(4)), tables(2), algo)
      )
      solver.add(cons)
    }

    if (stat.nSols != statRef.nSols) {
      println(algo + " " + stat.nSols + " " + statRef.nSols)
      tables(0).foreach(a => println(a.mkString(",")))
      println("")
    }

    assert(stat.nSols == statRef.nSols)
    assert(stat.nFails == statRef.nFails)
  }
}

