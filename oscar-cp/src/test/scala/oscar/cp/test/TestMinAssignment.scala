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
import oscar.cp._
import oscar.cp.constraints.MinAssignment

class TestMinAssignment extends FunSuite with ShouldMatchers {

  test("Test Assignment 1") {
    val cp = CPSolver()
    val w = Array(Array(2, 3, 4), 
                  Array(3, 3, 3), 
                  Array(4, 5, 2))
    val cost = CPIntVar(0 to 100)(cp)
    val x = Array.fill(3)(CPIntVar(0 to 2)(cp))
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(7)
    cp.post(x(0) != 0)
    cost.min should be(8)
    cp.post(x(2) != 2)
    cost.min should be(10)
  }
  

  test("Test Assignment 2") {
    val cp = CPSolver()
    val w = Array(Array(2, 3, 4), 
                  Array(3, 3, 3), 
                  Array(4, 5, 2))
    val cost = CPIntVar(0 to 100)(cp)
    val x = Array(CPIntVar(Set(1,2))(cp), CPIntVar(Set(0,1,2))(cp), CPIntVar(Set(0,1))(cp))
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(10)
  }
  
  
  test("Test Assignment 3") {
    val cp = CPSolver()
    val w = Array(Array(2, 3, 4), 
                  Array(3, 3, 3), 
                  Array(4, 5, 2))
    val cost = CPIntVar(0 to 100)(cp)
    val x = Array(CPIntVar(Set(0,1,2))(cp), CPIntVar(Set(0,1,2))(cp), CPIntVar(Set(0,1))(cp))
    cp.post(new MinAssignment(x, w, cost))
    //println(cost)
    cost.min should be(10)
  }  
  
  test("Test Assignment 4") {
    val cp = CPSolver()
    val w = Array(Array(2, 3, 4), 
                  Array(3, 3, 3), 
                  Array(4, 5, 2))
    val cost = CPIntVar(0 to 100)(cp)
    val x = Array.fill(3)(CPIntVar(0 to 2)(cp))
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(7)
    cp.pushState()
    cp.post(x(0) != 0)
    cost.min should be(8)
    cp.pop()
    cp.post(x(2) != 2)
    cost.min should be(10)
  }
  
  
  test("Test Assignment 5") {
    val cp = CPSolver()
    val w = Array(Array(2, 3, 4), 
                  Array(3, 3, 3), 
                  Array(4, 5, 2))
    val cost = CPIntVar(0 to 100)(cp)
    val x = Array.fill(3)(CPIntVar(0 to 2)(cp))
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(7)
    cp.post(cost <= 7)
    //println(x.mkString(","))
    x(0).value should be(0)
    //x(1).value should be(1) // if we do AC
    x(2).value should be(2)
  } 
  
  test("Test Assignment 6") {
    val cp = CPSolver()
    val w = Array(Array(0, 1, 2), 
                  Array(0, 0, 0), 
                  Array(2, 3, 0))
    val cost = CPIntVar(0 to 100)(cp)
    val x = Array.fill(3)(CPIntVar(0 to 2)(cp))
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(0)
    cp.post(cost <= 0)
    //println(x.mkString(","))
    x(0).value should be(0)
    //x(1).value should be(1) // if we do AC
    x(2).value should be(2)
  }
  
  test("Test Assignment 7") {
    val cp = CPSolver()
    val w = Array(Array(0, 1, 2, 3), 
                  Array(1, 1, 0, 0), 
                  Array(2, 3, 0, 3))
    val cost = CPIntVar(0 to 100)(cp)
    val x = Array.fill(3)(CPIntVar(0 to 3)(cp))
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(0)
    cp.post(cost <= 0)
    x(0).value should be(0)
    x(1).value should be(3)
    x(2).value should be(2)
  }    
  
   

}
