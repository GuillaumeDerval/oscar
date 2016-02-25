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

import oscar.algo.reversible.{SparseSet, ReversibleSparseSet, ReversibleInt, ReversibleBoolean}
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
 *
 * Implem of: STR2: optimized simple tabular reduction for table constraints, Christophe Lecoutre
 *
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


  // Stacks used to represent sSup et SVal
  // sSup is the uninstanciated variables whose domain contains at least one value for which a support has not yet been found
  // sVAL is the uninstanciated variables whose domain has been reduced since the previous invocation of STR2
  private[this] val sSup = new SparseSet(0,arity-1,true)
  private[this] val sVal = new Array[Int](arity)
  private[this] var sValSize = 0

  private[this] val gacValues = Array.tabulate(arity)(i => new SparseSet(variables(i).min,variables(i).max,true))

  private[this] val offsets = Array.tabulate(arity)(i => variables(i).min)
  private[this] val sizes = new Array[Int](arity)

  private[this] val unBoundVars = new ReversibleSparseSet(s,0,arity-1)
  private[this] val varIndices = Array.ofDim[Int](arity)
  private[this] val values = Array.ofDim[Int](variables.map(_.size).max)
  
  private[this] var nChanged = 0
  private[this] var changedIdx = 0

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

    // Step1: ----- Initialize and reset GAC values -------

    nChanged = 0
    // Reset SSup and SVal
    sSup.empty()
    sValSize = 0
    // Cache
    nActiveTuples = nActiveTuplesRev.value

    var i = unBoundVars.fillArray(varIndices)
    while (i > 0) {
      i -= 1
      val varIdx = varIndices(i)
      val varSize = variables(varIdx).size
      gacValues(varIdx).empty()
      sSup.insert(varIdx)
      val inSVal = lastSize(varIdx).value != varSize // changed since last propagate
      lastSize(varIdx).setValue(varSize)
      if (inSVal) {
        sVal(sValSize) = varIdx
        sValSize += 1 // push
        changedIdx = varIdx
        nChanged += 1
      }
    }

    // Step2: ----- invalidate tuples and compute GAC values -------

    i = nActiveTuples
    while (i > 0) {
      i -= 1
      val tau = table(activeTuples(i))
      val isInvalid = isInvalidTuple(tau)
      if (isInvalid) deactivateTuple(i)
      else {
        // tuple i is thus valid, we need to check every variable
        // for which at least one value has not a support yet (the ones in sSup)
        var j = sSup.fillArray(varIndices)
        while (j > 0) {
          j -= 1
          val varId = varIndices(j)
          // remove value tau(varId) from the value to be removed
          if (!gacValues(varId).hasValue(tau(varId))) {
            gacValues(varId).insert(tau(varId))
            if (gacValues(varId).getSize == variables(varId).size) {
              sSup.removeValue(varId)
            }
          }
        }
      }
    }
    // Not in STR2: no more tuples, so domains will be completely empty anyway
    if (nActiveTuples == 0) {
      return Failure
    }

    // Step3: ----- Filter the domains -------

    i = sSup.fillArray(varIndices)
    while (i > 0) {
      i -= 1
      val varId = varIndices(i)
      // Not in STR2: if varId was the only one that has changed, all its values are still consistant
      if (nChanged > 1 || changedIdx != varId) {
        val variable = variables(varId)
        val nGacValues = gacValues(varId).fillArray(values)
        if (nGacValues == 0) return Failure
        else if (nGacValues == 1) {
          variable.assign(values(0))
          unBoundVars.removeValue(varId)
        } else {
          var varSize = variable.fillArray(values)
          var j = varSize
          while (j > 0) {
            j -= 1
            val v = values(j)
            if (!gacValues(varId).hasValue(v)) {
              variable.removeValue(v)
              varSize -= 1
            }
          }
          lastSize(varId).setValue(varSize)
          if (varSize == 1) {
            unBoundVars.removeValue(varId)
          }
        }
      }
    }

    // Trail only if no Failure
    nActiveTuplesRev.value = nActiveTuples

    return Suspend
  }


  def isInvalidTuple(tuple: Array[Int]): Boolean = {
    var i = sValSize
    while (i > 0) {
      i -= 1
      val varId = sVal(i)
      if (!variables(varId).hasValue(tuple(varId))) return true
    }
    false
  }



  def swapTuple(id1: Int,id2: Int): Unit = {
    val tmpPosition = activeTuples(id1)
    activeTuples(id1) = activeTuples(id2)
    activeTuples(id2) = tmpPosition
  }

 def deactivateTuple(id: Int): Unit = {
    nActiveTuples -= 1
    val tmpPosition = activeTuples(id)
    activeTuples(id) = activeTuples(nActiveTuples)
    activeTuples(nActiveTuples) = tmpPosition
 }
}