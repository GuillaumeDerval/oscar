package solvers.cp.branchings

import models.CPModel
import solvers.cp.branchings.Branching.Alternative
import vars.IntVar

/**
  * Last Conflict Search
  *
  * Last confict is a first-fail best-firt search.
  *
  * Variables are firstly assigned according to varHeuristic
  * and then to the last conflict scheme.
  *
  * Each variable is assigned to its last successfully assigned value
  * or the value returned by valHeuristic
  *
  * @author Steven Gay
  * @author Renaud Hartert
  * @author Guillaume Derval
  */

class BinaryLastConflict(cp: CPModel, variables: Array[IntVar], varHeuristic: Int => Int, valHeuristic: Int => Int) extends Branching {

  require(variables.length > 0, "no variable")

  private[this] val nVariables = variables.length

  // Order in which variables have to be assigned
  private[this] val order = Array.tabulate(nVariables) { i => i }

  // Last successful assigned value for each variable
  private[this] val lastValues = Array.fill(nVariables)(Int.MinValue)

  // Current depth of the search tree
  private[this] val nAssignedRev = cp.getReversibleInt(0)

  // Last conflict
  private[this] var maxAssigned: Int = -1
  private[this] var conflictAssign: Int = 0

  final override def reset(): Unit = maxAssigned = -1

  final override def alternatives: Seq[Alternative] = {
    val nAssigned = updateAssigned()
    if (nAssigned >= nVariables) Branching.noAlternative
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
  @inline private def assign(variable: IntVar, value: Int, nAssigned: Int): Alternative = () => {
    val out = cp.post(variable === value)
    if (!out) conflictAssign = nAssigned
  }

  // Return an Alternative that assign the value to the variable
  @inline private def remove(variable: IntVar, value: Int, nAssigned: Int): Alternative = () => {
    val out = cp.post(variable !== value)
    if (!out) conflictAssign = nAssigned
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