package oscar.algo.branchings

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

import oscar.algo.reversible._
import oscar.algo.search.Outcome.Failure
import oscar.algo.search.{Branching, _}
import oscar.algo.vars.IntVarLike

/**
 * Last Conflict Search
 * 
 * Last confict is a first-fail best-firt search.
 *  
 * Variables are firstly assigned according to varHeuristic 
 * and then to the last conflict scheme.
 * 
 * Each variable is assigned to its last successfuly assigned value
 * or the value returned by valHeuristic
 *
 * @author Steven Gay
 * @author Renaud Hartert
 */

class BinaryLastConflict(variables: Array[IntVarLike], varHeuristic: Int => Int, valHeuristic: Int => Int) extends Branching {

  require(variables.length > 0, "no variable")

  private[this] val nVariables = variables.length
  private[this] val context = variables(0).context

  // Order in which variables have to be assigned
  private[this] val order = Array.tabulate(nVariables) { i => i }

  // Last successful assigned value for each variable
  private[this] val lastValues = Array.fill(nVariables)(Int.MinValue)

  // Current depth of the search tree
  private[this] val nAssignedRev = new ReversibleInt(context, 0)

  // Last conflict
  private[this] var maxAssigned: Int = -1
  private[this] var conflictAssign: Int = 0

  final override def reset(): Unit = maxAssigned = -1

  final override def alternatives: Seq[Alternative] = {
    val nAssigned = updateAssigned()
    if (nAssigned >= nVariables) noAlternative
    else {

      // Trail the new depth
      nAssignedRev.value = nAssigned
      
      // Select the variable implied in the last conflict if any
      if (conflictAssign > nAssigned && !variables(order(conflictAssign)).isBound) {
        val deepestId = order(conflictAssign)
        // Insert the last conflict in the sequence
        System.arraycopy(order, nAssigned, order, nAssigned + 1, conflictAssign - nAssigned)
        order(nAssigned) = deepestId
        conflictAssign = -1
      } 
      // Select the next variable suggested by the variable heuristic
      else if (nAssigned > maxAssigned) {
        maxAssigned = nAssigned
        val position = nextVariable(nAssigned)
        val varId = order(position)
        // Swap the next variable
        order(position) = order(nAssigned)
        order(nAssigned) = varId
      }

      val varId = order(nAssigned)
      val variable = variables(varId)
      val lastValue = lastValues(varId)
      val value = if (variable.hasValue(lastValue)) lastValue else valHeuristic(varId)
      // Alternatives
      List(assign(variable, value, nAssigned), remove(variable, value, nAssigned))
    }
  }
  
  // Return an Alternative that assign the value to the variable
  @inline private def assign(variable: IntVarLike, value: Int, nAssigned: Int): Alternative = () => {
    val out = context.assign(variable, value)
    if (out == Failure) conflictAssign = nAssigned
  }
  
  // Return an Alternative that assign the value to the variable
  @inline private def remove(variable: IntVarLike, value: Int, nAssigned: Int): Alternative = () => {
    val out = context.remove(variable, value)
    if (out == Failure) conflictAssign = nAssigned
  }

  @inline private def updateAssigned(): Int = {
    var d = nAssignedRev.value
    while (d < nVariables && variables(order(d)).isBound) {
      val varId = order(d)
      lastValues(varId) = variables(varId).min
      d += 1
    }
    d
  }

  @inline private def nextVariable(depth: Int): Int = {
    var minId = depth
    var min = Int.MaxValue
    var i = depth
    while (i < nVariables) {
      val varId = order(i)
      if (!variables(varId).isBound) {
        val m = varHeuristic(order(i))
        if (m < min) {
          min = m
          minId = i
        }
      }
      i += 1
    }
    minId
  }
}
