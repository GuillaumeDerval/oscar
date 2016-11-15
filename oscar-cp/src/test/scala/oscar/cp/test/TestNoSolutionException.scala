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

import scala.reflect.Manifest

import org.scalatest.FunSuite
import org.scalatest.events.Formatter
import org.scalatest.ShouldMatchers

import oscar.cp._

class TestNoSolutionException extends FunSuite with ShouldMatchers {
	
	test("test1: using add outside subjectTo allows NoSolutionExceptions") {
	  val cp = CPSolver()
	  val x = CPIntVar(Array(10, 20, 30))(cp)
	  
	  intercept[NoSolutionException]{ 
	    cp.add(x < 10)
	  }  
	  
	  cp.isFailed should be(true)
	}
	
	test("test3: using post outside subjectTo shouldn't generate NoSolutionExceptions") {
	  val cp = CPSolver()
	  val x = CPIntVar(Array(10, 20, 30))(cp)
	  
	  try {
	    cp.post(x < 10)
	  }  catch {
	    case e: NoSolutionException => fail("post should handle the NoSolutionException internally")
	  }
	  
	  cp.isFailed should be(true)
	}
	
	test("test4: using post inside subjectTo shouldn't generate NoSolutionExceptions") {
	  val cp = CPSolver()
	  val x = CPIntVar(Array(10, 20, 30))(cp)
	  try {
			cp.post(x < 10)
	  } catch {
	    case e: NoSolutionException => fail("The NoSolutionException should be caught by the subjectTo block")
	  }
	  cp.isFailed should be(true)
	}
}
