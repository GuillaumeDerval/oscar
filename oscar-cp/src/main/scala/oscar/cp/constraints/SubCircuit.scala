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

package oscar.cp.constraints

import oscar.cp.core.variables.CPIntVar
import oscar.cp.modeling._
import oscar.cp.core.Constraint
import oscar.algo.reversible.ReversibleInt
import oscar.cp._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

/**
 * SubCircuit
 *
 * @author Renaud Hartert ren.hartert@gmail.com
 * @author Pierre Schaus pschaus@gmail.com
 */
final class SubCircuit(successors: Array[CPIntVar]) extends Constraint(successors(0).store, "SubCircuit") {

  private[this] val nSuccessors = successors.length
  private[this] val dest: Array[ReversibleInt] = Array.tabulate(nSuccessors)(new ReversibleInt(s, _))
  private[this] val src: Array[ReversibleInt] = Array.tabulate(nSuccessors)(new ReversibleInt(s, _))
  private[this] val nSubCircuits: ReversibleInt = new ReversibleInt(s, 0)

  override def setup(l: CPPropagStrength): CPOutcome = {
    if (s.post(new AllDifferent(successors: _*), l) == Failure) Failure
    else if (init() == Failure) Failure
    else Suspend
  }

  private def init(): CPOutcome = {
    var i = nSuccessors
    while (i > 0) {
      i -= 1
      val idx = i
      val successor = successors(i)
      if (!successor.isBound) {
        successor.callValBindIdxWhenBind(this, i)
        successor.filterWhenBind(true,CPStore.MaxPriorityL2) {
          bind(idx)
        }
      }
      else if (bind(idx) == Failure) return Failure
    }
    Suspend
  }

   private def close(): CPOutcome = {
    var i = nSuccessors
    while (i > 0) {
      i -= 1
      val successor = successors(i)
      if (!successor.isBound && successor.assign(i) == Failure) return Failure
    }
    Success // All the variables are bound
  }

  private def bind(u: Int): CPOutcome = {
    // s ->* u -> v ->* d
    val v = successors(u).min
    if (u == v) Suspend
    else {    
      val s = src(u).value
      val d = dest(v).value
      src(d).value = s
      dest(s).value = d     
      // Updates the number of sub-circuits
      if (d != v) nSubCircuits.decr()
      if (s == u) nSubCircuits.incr()
      // Only one sub-circuit
      if (nSubCircuits.value > 1) {
        if (successors(d).removeValue(s) == Failure) {
          return Failure
        }
      }     
      // Closes the circuit
      if (src(u).value == v) close()
      else Suspend
    }
  }
}

object SubCircuit {
  def apply(successors: Array[CPIntVar], offset: Int = 0): SubCircuit = {
    val succs: Array[CPIntVar] = if (offset == 0) successors else successors.map(_-offset)
    new SubCircuit(succs)
  }
}