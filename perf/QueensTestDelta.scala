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

import oscar.cp._
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPSolver
import oscar.cp.util.ArrayUtils
import oscar.algo.reversible.ReversibleInt

/**
 * n-queens model: place n-queens on a chess-board such that they don't attack each other.
 * this program search for all the solutions
 * Using Non Deterministic Search
 * @author Pierre Schaus pschaus@gmail.com
 */
object QueensTestDelta {

  /*
  class Cons1(val x: Array[CPIntVar]) extends Constraint(x(0).store, "Cons1") {

    //val  = x.map(_.size).max
    val delta = Array.tabulate(x.size)(i => Array.ofDim[Int](x(i).size))
    var currDim = Array.tabulate(x.size)(i => 0)

    override def setup(l: CPPropagStrength): CPOutcome = {

      for (i <- 0 until x.size) {
        x(i).callPropagateWhenDomainChanges(this, true)
        x(i).callValRemoveIdxWhenValueIsRemoved(this, i)
      }
      propagate()

    }

    s.onPop {
      for (i <- 0 until x.size) {
        currDim(i) = 0
      }
    }

    override def valRemoveIdx(x: CPIntVar, idx: Int, value: Int) = {
      delta(idx)(currDim(idx)) = value
      currDim(idx) += 1
      CPOutcome.Suspend
    }

    override def propagate(): CPOutcome = {
      for (i <- 0 until x.size) {
        var j = 0
        while (j < currDim(i)) {
          delta(i)(j)
          j += 1
        }
      }

      for (i <- 0 until x.size) {
        currDim(i) = 0
      }
      CPOutcome.Suspend
    }
  }
  
  
  class Cons2(val x: Array[CPIntVar]) extends Constraint(x(0).store, "Cons1") {
    
    val delta = Array.tabulate(x.size)(i => Array.ofDim[Int](x(i).size))
    var currDim = Array.tabulate(x.size)(i => 0)
    
    override def setup(l: CPPropagStrength): CPOutcome = {

      for (i <- 0 until x.size) {
        x(i).callPropagateWhenDomainChanges(this, true)
      }
      propagate()

    }
 
    
    override def propagate(): CPOutcome = {
      for (i <- 0 until x.size) {
        //x(i).delta(this)
        //println("here")
        
        
        val m = x(i).fillDeltaArray(this, delta(i))
        var j = 0
        while (j < m) {
          delta(i)(j)
          j += 1
        }
        
        
      }
      CPOutcome.Suspend
    }
  } 
  */
  
  class Cons3(val x: Array[CPIntVar]) extends Constraint(x(0).store, "Cons1") {
    
    val delta = Array.tabulate(x.size)(i => Array.ofDim[Int](x(i).size))
    var currDim = Array.tabulate(x.size)(i => 0)
    
    override def setup(l: CPPropagStrength): CPOutcome = {
      for (i <- 0 until x.size) {
       x(i).filterWhenDomainChanges { d =>
          val m = d.fillArray(delta(i))
          var j = 0
          while (j < m) {
            delta(i)(j)
            j += 1
          }
          CPOutcome.Suspend
        }
      }
      CPOutcome.Success
    }
  }   

  def main(args: Array[String]) {

    implicit val cp = CPSolver()
    cp.silent = true
    val n = 88 //number of queens
    val Queens = 0 until n
    //variables
    val queens = for (i <- Queens) yield CPIntVar(cp, 1 to n)

    var nbsol = 0
    
    cp.add(new Cons3(queens))
    cp.add(new Cons3(for (i <- Queens) yield queens(i) + i))
    cp.add(new Cons3(for (i <- Queens) yield queens(i) - i))     
    
    cp.add(allDifferent(queens), Strong)
    cp.add(allDifferent(for (i <- Queens) yield queens(i) + i), Strong)
    cp.add(allDifferent(for (i <- Queens) yield queens(i) - i), Strong)

    // Search heuristic
    search(binaryFirstFail(queens))

    // Execution
    val stats = start(nSols = 1)
    println(stats)

  }
}


