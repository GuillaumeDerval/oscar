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
import oscar.cp.constraints._
import oscar.cp._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import oscar.cp.constraints.tables.TableAlgo
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.ShouldMatchers

/**
 * @author: Pierre Schaus (pschaus@gmail.com)
 */

//@RunWith(classOf[JUnitRunner])
class TestTable extends FunSuite with ShouldMatchers {


  val rand = new scala.util.Random(16)

  def randomTuples(dim: Int, n: Int, minValue: Int, maxValue: Int) = {
    Array.fill(n, dim)(rand.nextInt(maxValue - minValue) + minValue)
  }


  test("Table Test") {
    for (i <- 0 until 1000) {

      val cp = CPSolver()
      var x = Array.fill(5)(CPIntVar(1 to 8)(cp))

      val tuples1 = randomTuples(3, 100, 3, 8)
      val tuples2 = randomTuples(3, 100, 2, 7)
      val tuples3 = randomTuples(3, 100, 1, 6)

      cp.add(allDifferent(x))
      
      cp.search {
        binaryFirstFail(x, _.max)
      }

      
      
      val statRef = cp.startSubjectTo() {
        
        val cons = Seq(new TableDecomp(Array(x(0), x(1), x(2)), tuples1),new TableDecomp(Array(x(2), x(3), x(4)), tuples2),new TableDecomp(Array(x(0), x(2), x(4)), tuples3))
        cp.add(cons)
      }

      for (algo <- TableAlgo.values) {
        val stat = cp.startSubjectTo() {
          val cons = Seq(table(Array(x(0), x(1), x(2)), tuples1, algo), table(Array(x(2), x(3), x(4)), tuples2, algo), table(Array(x(0), x(2), x(4)), tuples3, algo))
          cp.add(cons)

        }
        if (stat.nSols != statRef.nSols) {
          println(algo+ " "+ stat.nSols+" "+statRef.nSols)
          tuples1.foreach(a => println(a.mkString(",")))
          println("")
        }
        assert(stat.nSols == statRef.nSols)
        assert(stat.nFails == statRef.nFails)
      }

    }
  }


}

