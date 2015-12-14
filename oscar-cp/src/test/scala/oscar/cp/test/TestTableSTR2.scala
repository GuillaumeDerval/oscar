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
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestTableSTR2 extends FunSuite with ShouldMatchers  {


  test("Table Test 1") {
    val cp = CPSolver()
    var x = Array.fill(3)(CPIntVar(1 to 3)(cp))
    
    val tuples = Array(Array(1,1,1),Array(1,2,3))
    

    cp.post(new TableSTR2(x,tuples))
    	
    x(0).isBound should be(true)
    x(0).value should be(1)
    x(2).hasValue(2) should be (false)
    
    cp.post(x(2) != 3)
    
    cp.isFailed should be(false)
    x(1).value should be(1)
    x(2).value should be(1)

  }
  
  test("Table Test 2") {
    val cp = CPSolver()
    
    var x = CPIntVar(0 to 4)(cp)
    var y = CPIntVar(0 to 4)(cp)
    var z = CPIntVar(0 to 24)(cp)
    
    
    val tuples = (for (i <- 0 until 5; j <- i+1 until 5) yield Array(i,j,i*4+j-1)).toArray
    cp.post(new TableSTR2(Array(x,y,z),tuples))
    cp.post(z == 0)
    x.value should be(0)
    y.value should be(1)
    z.value should be(0)

  }
  
  test("Table Test 3") {
    val cp = CPSolver()
    var x = Array.fill(3)(CPIntVar(1 to 7)(cp))
    
    val tuples = Array(Array(1,1,1),Array(1,2,3),Array(1,2,7),Array(2,1,4))
    

    
    
    var nbSol = 0
    	

      cp.add(new TableSTR2(x,tuples))
    cp.search {
      binaryStatic(x)
    } onSolution {
      nbSol += 1
    }start()
    nbSol should be(4)

  }
  
    test("Table Test 4") {
    val cp = CPSolver()
    var x = Array.fill(2)(CPIntVar(1 to 1)(cp))
    
    val tuples = Array(Array(1,2),Array(2,1))
    

    cp.post(new TableSTR2(x,tuples))
    cp.isFailed should be(true)
    

  }
    
    test("Table Test 5") {
    implicit val cp = CPSolver()
    var x = Array.fill(6)(CPIntVar(2 to 3)(cp))
    var nbSol = 0
    
    val tuples = Array(
        Array(2,2,3,2,2,3),
        Array(2,3,2,2,3,2)
        )
    

    cp.post(new TableSTR2(x,tuples))
    cp.search(binaryStatic(x))
    cp.onSolution {
      //println(x.mkString(", "))
      nbSol += 1
    }
    cp.start()
    nbSol should be(2)
    

  }
    
    test("Table Test 6") {
    implicit val cp = CPSolver()
    var x = Array.fill(6)(CPIntVar(0 to 5)(cp))
    var nbSol = 0
    
    val tuples = Array(
        Array(5, 5, 0, 5, 1, 4),
        Array(5, 5, 0, 3, 1, 1),
        Array(3, 1, 4, 2, 3, 1)
        )
    

    cp.post(new TableSTR2(x,tuples))
    cp.search(binaryStatic(x))
    cp.onSolution {
      //println(x.mkString(", "))
      nbSol += 1
    }
    cp.start()
    nbSol should be(3)
    

  }
    
    test("Table Test 7") {
    implicit val cp = CPSolver()
    var x = Array.fill(7)(CPIntVar(0 to 3)(cp))
    var nbSol = 0
    
    val tuples = Array(
		Array(1, 0, 3, 1, 0, 2, 1),
		Array(1, 3, 3, 3, 2, 1, 3),
		Array(1, 0, 3, 0, 0, 3, 1)
        )
    
    cp.post(new TableSTR2(x,tuples))
    cp.search(binaryStatic(x))
    cp.onSolution {
      //println(x.mkString(", "))
      nbSol += 1
    }
    cp.start()
    nbSol should be(3)
    

  }
    
}
