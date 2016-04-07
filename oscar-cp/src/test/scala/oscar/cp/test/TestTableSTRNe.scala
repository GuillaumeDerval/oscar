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
import oscar.cp.constraints.tables.TableSTRNe
import oscar.cp._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestTableSTR2Ne extends FunSuite with ShouldMatchers  {
  
  test("Table Test 1") {
    /*
     * x0 = x1 = x2 = {1, 2}
     * 0 1 2
     * -----
     * 1 1 1
     * 1 1 2
     * 1 2 1
     * 1 2 2
     * 
     * x0 must be bound to 2
     */
    
    val cp = CPSolver()
    var x = Array.fill(3)(CPIntVar(1 to 2)(cp))
    val tuples = Array(Array(1,1,1),Array(1,1,2),Array(1,2,1),Array(1,2,2))

    cp.post(new TableSTRNe(x,tuples))
    x(0).isBound should be(true)
    x(0).value should be(2)
    
  }
  
  test("Table Test 2") {
    val cp = CPSolver()
    var x = CPIntVar(1 to 4)(cp)
    var y = CPIntVar(1 to 4)(cp)
    var z = CPIntVar(1 to 4)(cp)
    
    val tuples = (for (i <- 1 to 3; j <- 1 to 4; k <- 1 to 4) yield Array(i,j,k)).toArray
    
    cp.post(new TableSTRNe(Array(x,y,z),tuples))
    x.value should be(4)
    
    var nbSol = 0
    cp.search {
      binaryStatic(Seq(x,y,z))
    } onSolution {
      nbSol += 1
    }start()
    nbSol should be(16)
    
  }
  
  test("Table Test 3") {
    val cp = CPSolver()
    var x = CPIntVar(0 to 10)(cp)
    var y = CPIntVar(5 to 100)(cp)
    var z = CPIntVar(0 to 10)(cp)
    val tuples = (for (i <- 0 to 9; j <- 5 to 100; k <- 0 to 10) yield Array(i,j,k)).toArray
    
    cp.post(new TableSTRNe(Array(x,y,z),tuples))
    x.value should be(10)
    
    var nbSol = 0
    cp.search {
      binaryStatic(Seq(x,y,z))
    } onSolution {
      nbSol += 1
    }start()
    nbSol should be(96 * 11)
  }
  
  test("Table Test 4") {
    val cp = CPSolver()
    var x = CPIntVar(0 to 10)(cp)
    var y = CPIntVar(5 to 100)(cp)
    var z = CPIntVar(0 to 10)(cp)
    var t = CPIntVar(0 to 50)(cp)
    val tuples = (for (i <- 0 to 10; j <- 5 to 100; k <- 0 to 10; m <- 0 to 50) yield Array(i,j,k,m)).toArray

    cp.post(new TableSTRNe(Array(x,y,z,t),tuples))
    cp.isFailed should be(true)
  }
  
  test("Table Test 5") {
    val cp = CPSolver()
    var x = Array.fill(3)(CPIntVar(1 to 7)(cp))
    val tuples = Array(Array(1,1,1),Array(1,2,3),Array(1,2,7),Array(2,1,4))
    var nbSol = 0

    cp.add(new TableSTRNe(x,tuples))
    cp.search {
      binaryStatic(x)
    } onSolution {
      nbSol += 1
    }start()
    nbSol should be(7 * 7 * 7 - 4)

  }  
  
  test("Table Test 6") {
    val cp = CPSolver()
    var x = Array.fill(2)(CPIntVar(1 to 10)(cp))
    val tuples = Array(Array(12,2),Array(2,12))
    var nbSol = 0
    
    cp.post(new TableSTRNe(x,tuples))
    cp.search {
      binaryStatic(x)
    } onSolution {
      nbSol += 1
    }start()
    nbSol should be(100)
  }
  
  test("Table Test 7") {
    val cp = CPSolver()
    var x = Array.fill(5)(CPIntVar(1 to 10)(cp))
    val tuples = (for (i1 <- 1 to 10; i2 <- 1 to 10; i3 <- 1 to 10; i4 <- 1 to 10; i5 <- 1 to 10) yield Array(i1,i2,i3,i4,i5)).toArray
    var nbSol = 0
    
    cp.post(new TableSTRNe(x,tuples))
    cp.search {
      binaryStatic(x)
    } onSolution {
      nbSol += 1
    }start()
    nbSol should be(0)
  }
  
  test("Table Test 8") {
    val cp = CPSolver()
    var x = Array.fill(5)(CPIntVar(1 to 10)(cp))
    val tuples = (for (i1 <- 1 to 9; i2 <- 1 to 10; i3 <- 1 to 10; i4 <- 1 to 10; i5 <- 1 to 10) yield Array(i1,i2,i3,i4,i5)).toArray
    var nbSol = 0
    
    cp.post(new TableSTRNe(x,tuples))
    x(0).value should be(10)
    
    cp.search {
      binaryStatic(x)
    } onSolution {
      nbSol += 1
    }start()
    nbSol should be(10*10*10*10)
  }
  
  test("Table Test 9") {
    val cp = CPSolver()
    var x = Array.fill(6)(CPIntVar(2 to 3)(cp))
    var nbSol = 0
    
    val tuples = Array(
        Array(2,2,3,2,2,3),
        Array(2,3,2,2,3,2)
        )
    
    cp.post(new TableSTRNe(x,tuples))
    cp.search(binaryStatic(x))
    cp.onSolution {
      //println(x.mkString(", "))
      nbSol += 1
    }
    cp.start()
    nbSol should be(2*2*2*2*2*2-2)

  }
  
  test("Table Test 10") {
    val cp = CPSolver()
    var x = Array.fill(6)(CPIntVar(0 to 5)(cp))
    var nbSol = 0
    
    val tuples = Array(
        Array(5, 5, 0, 5, 1, 4),
        Array(5, 5, 0, 3, 1, 1),
        Array(3, 1, 4, 2, 3, 1)
        )

    cp.post(new TableSTRNe(x,tuples))
    cp.search(binaryStatic(x))
    cp.onSolution {
      //println(x.mkString(", "))
      nbSol += 1
    }
    cp.start()
    nbSol should be(6*6*6*6*6*6-3)
  }
  
  test("Table Test 11") {
    implicit val cp = CPSolver()
    var x = Array.fill(7)(CPIntVar(0 to 3)(cp))
    var nbSol = 0
    
    val tuples = Array(
		Array(1, 0, 3, 1, 0, 2, 1),
		Array(1, 3, 3, 3, 2, 1, 3),
		Array(1, 0, 3, 0, 0, 3, 1)
        )
    
    cp.post(new TableSTRNe(x,tuples))
    cp.search(binaryStatic(x))
    cp.onSolution {
      //println(x.mkString(", "))
      nbSol += 1
    }
    cp.start()
    nbSol should be(4*4*4*4*4*4*4-3)

  }
   
}
