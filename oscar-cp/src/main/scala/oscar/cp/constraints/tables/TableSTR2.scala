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

package oscar.cp.constraints

import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.ReversibleBoolean
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.CPStore
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.variables.CPIntVarAdaptable
import scala.util.control._

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 * @author Sacha Van Cauwelaert sascha.vancauwelaert@gmail.com
 * @author Jordan Demeulenaere
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 * @author Guillaume Perez memocop@gmail.com
 */
final class TableSTR2(variables: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(variables(0).store, "TableSTR2") {

  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1

  private[this] val arity = variables.length
  private[this] val nTuples = table.length

  // Tuples to consider
  private[this] val activeTuples = Array.tabulate(nTuples)(i => i)
  private[this] val nActiveTuplesRev = new ReversibleInt(s, nTuples)
  private[this] var nActiveTuples = 0

  private[this] val isBoundAndChecked = Array.fill(arity)(new ReversibleBoolean(s, false))

  // Stacks used to represent SSup et SVal
  private[this] val sSup = new Array[Int](arity)
  private[this] val sVal = new Array[Int](arity)
  private[this] var sSupSize = 0
  private[this] var sValSize = 0

  // Sparse sets for values to remove
  private[this] val toRemoveValues = Array.tabulate(arity)(i => new Array[Int](variables(i).size))
  private[this] val toRemovePositions = Array.tabulate(arity)(i => new Array[Int](variables(i).max - variables(i).min + 1))
  private[this] val offsets = Array.tabulate(arity)(i => variables(i).min)
  private[this] val sizes = new Array[Int](arity)

  /////////
  // POUR V2
  /////////
  private[this] var valModified  = false

  // Last size of the domain
  private[this] val lastSize = Array.fill(arity)(new ReversibleInt(s, -1))

  override def setup(l: CPPropagStrength): CPOutcome = {
    if (propagate() == Failure) Failure
    else {
      var i = arity
      while (i > 0) {
        i -= 1
        if (!variables(i).isBound) variables(i).callPropagateWhenDomainChanges(this)
      }
      Suspend
    }
  }

  override def propagate(): CPOutcome = {

    // Reset SSup and SVal
    sSupSize = 0
    sValSize = 0

    // Cache
    nActiveTuples = nActiveTuplesRev.value

    var i = arity
    while (i > 0) {
      i -= 1
      if (!isBoundAndChecked(i).value) {
        updateSet(i) // Copy the domain of the variable
        sSup(sSupSize) = i
        sSupSize += 1 // push
        val varSize = variables(i).size
        val inSVal = lastSize(i).value != varSize // changed since last propagate
        lastSize(i).value = varSize
        if (inSVal) {
          sVal(sValSize) = i
          sValSize += 1 // push
        }
      }
    }

    //////
    // V2
    /////
    //    var irs = 0
    //    i = 0
    //    while (i < nActiveTuples && sSupSize != 0) {
    //      val tau = table(activeTuples(i))
    //      val isInvalid = isInvalidTuple(tau)
    //      if (isInvalid) {
    //        deactivateTuple(i)
    //        i -= 1
    //      }
    //      else {
    //        var j = sSupSize
    //        valModified = false
    //        while (j > 0) {
    //          j -= 1
    //          val varId = sSup(j)
    //          val newSize = removeFromSet(varId, tau(varId))
    //          if (newSize == 0) {
    //            removeFromSSup(j)
    //          }
    //        }
    //        if(valModified){
    //          swapTuple(irs,i)
    //          irs += 1
    //        }
    //      }
    //      i += 1
    //    }
    //    while (i < nActiveTuples) {
    //      val tau = table(activeTuples(i))
    //      val isInvalid = isInvalidTuple(tau)
    //      if (isInvalid) {
    //        deactivateTuple(i)
    //        i -= 1
    //      }
    //      i += 1
    //    }

    ///////
    // V1
    ///////
    i = nActiveTuples
    while (i > 0 && sSupSize != 0) {
      i -= 1
      val tau = table(activeTuples(i))
      val isInvalid = isInvalidTuple(tau)
      if (isInvalid) deactivateTuple(i)
      else {
        var j = sSupSize
        while (j > 0) {
          j -= 1
          val varId = sSup(j)
          val newSize = removeFromSet(varId, tau(varId))
          if (newSize == 0) {
            removeFromSSup(j)
          }
        }
      }
    }
    while (i > 0) {
      i -= 1
      val tau = table(activeTuples(i))
      if (isInvalidTuple(tau)) deactivateTuple(i)
    }

    ////////
    // V0
    ///////
    //    i = nActiveTuples
    //    while (i > 0) {
    //      i -= 1
    //      val tau = table(activeTuples(i))
    //      val isInvalid = isInvalidTuple(tau)
    //      if (isInvalid) deactivateTuple(i)
    //      else {
    //        var j = sSupSize
    //        while (j > 0) {
    //          j -= 1
    //          val varId = sSup(j)
    //          val newSize = removeFromSet(varId, tau(varId))
    //          if (newSize == 0) removeFromSSup(j)
    //        }
    //      }
    //    }


    i = sSupSize
    while (i > 0) {
      i -= 1
      val varId = sSup(i)
      val values = toRemoveValues(varId)
      val nValues = sizes(varId)
      val variable = variables(varId)
      val varSize = variable.size

      if (nValues == varSize) return Failure
      else {
        if (nValues > 0) {
          var i = nValues
          while (i > 0) {
            i -= 1
            val value = values(i)
            variable.removeValue(value)
          }
        }
        if (variable.isBound) isBoundAndChecked(varId).setTrue()
        lastSize(varId).setValue(varSize)
      }
    }

    // Trail only if no Failure
    nActiveTuplesRev.value = nActiveTuples

    Suspend
  }

  @inline private def updateSet(varId: Int): Unit = {
    val variable = variables(varId)
    // Copy the values
    val size = variable.fillArray(toRemoveValues(varId))
    // Compute the positions
    val values = toRemoveValues(varId)
    val positions = toRemovePositions(varId)
    val offset = offsets(varId)
    sizes(varId) = size
    var i = size
    while (i > 0) {
      i -= 1
      val value = values(i)
      positions(value - offset) = i
    }
  }

  @inline private def removeFromSet(varId: Int, val1: Int): Int = {
    val positions = toRemovePositions(varId)
    val offset = offsets(varId)
    val pos1 = positions(val1 - offset)
    val size = sizes(varId)
    if (pos1 >= size) size
    else {
      val values = toRemoveValues(varId)
      val pos2 = size - 1
      val val2 = values(pos2)
      values(pos1) = val2
      values(pos2) = val1
      positions(val1 - offset) = pos2
      positions(val2 - offset) = pos1
      sizes(varId) = pos2
      valModified = true
      pos2
    }
  }

  @inline private def isInvalidTuple(tuple: Array[Int]): Boolean = {
    var i = sValSize
    while (i > 0) {
      i -= 1
      val varId = sVal(i)
      if (!variables(varId).hasValue(tuple(varId))) return true
    }
    false
  }

  @inline private def removeFromSSup(id: Int): Unit = {
    val tmp = sSup(id)
    sSupSize -= 1
    sSup(id) = sSup(sSupSize)
    sSup(sSupSize) = tmp
  }

  @inline private def swapTuple(id1: Int,id2: Int): Unit = {
    val tmpPosition = activeTuples(id1)
    activeTuples(id1) = activeTuples(id2)
    activeTuples(id2) = tmpPosition
  }

  @inline private def deactivateTuple(id: Int): Unit = {
    nActiveTuples -= 1
    val tmpPosition = activeTuples(id)
    activeTuples(id) = activeTuples(nActiveTuples)
    activeTuples(nActiveTuples) = tmpPosition
  }
}