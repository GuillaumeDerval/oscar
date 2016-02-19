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
 *
 * Implem of: STR2: optimized simple tabular reduction for table constraints, Christophe Lecoutre
 *
 */


object cpt {
  var c = 0
}
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
  private[this] val sSup = new Array[Int](arity)

  private[this] val sVal = new Array[Int](arity)
  private[this] var sSupSize = 0
  private[this] var sValSize = 0

  // Sparse sets for values to remove
  private[this] val toRemoveValues = Array.tabulate(arity)(i => new Array[Int](variables(i).size))
  private[this] val toRemovePositions = Array.tabulate(arity)(i => new Array[Int](variables(i).max - variables(i).min + 1))

  private[this] val offsets = Array.tabulate(arity)(i => variables(i).min)
  private[this] val sizes = new Array[Int](arity)

  private[this] val gac = Array.tabulate(arity)(i => new Array[Int](variables(i).max - variables(i).min + 1))
  private[this] var timeStamp = 0

  private[this] val unBoundVars = Array.tabulate(arity)(i => i)
  private[this] val unBoundVarsSize = new ReversibleInt(s,arity)


  /////////
  // POUR V2
  /////////
  private[this] var valModified  = false

  // Last size of the domain
  private[this] val lastSize = Array.fill(arity)(new ReversibleInt(s, -1))
  //private[this] val lastSize = Array.fill(arity)(-1)

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

  def initStructure(): Unit = {
    // Reset SSup and SVal
    sSupSize = 0
    sValSize = 0

    // Cache
    nActiveTuples = nActiveTuplesRev.value
    var i = arity
    while (i > 0) {
      i -= 1
        updateSet(i) // Copy the domain of the variable
        sSup(sSupSize) = i
        sSupSize += 1 // push
        val varSize = variables(i).size
        val inSVal = lastSize(i).value != varSize // changed since last propagate
        lastSize(i).setValue(varSize)
        if (inSVal) {
          sVal(sValSize) = i
          sValSize += 1 // push
        }
    }
  }


  override def propagate(): CPOutcome = {

    //println(arity)

    // side effect of increasing the timestamp is to invalidate all the gac
    /*
    cpt.c += 1
    if (cpt.c % 3000 == 0) {
      println(cpt.c)
    }*/

    timeStamp += 1


    initStructure()

    ////////
    // V0
    ///////
    var i = nActiveTuples
    while (i > 0) {
      i -= 1
      val tau = table(activeTuples(i))
      val isInvalid = isInvalidTuple(tau)
      if (isInvalid) deactivateTuple(i)
      else {
        // tuple i is thus valid, we need to check every variable
        // for which at least one value has not a support yet (the ones in sSup)
        var j = sSupSize
        while (j > 0) {
          j -= 1
          val varId = sSup(j)
          // remove value tau(varId) from the value to be removed
          if (gac(varId)(tau(varId)-offsets(varId)) != timeStamp) {
            gac(varId)(tau(varId)-offsets(varId)) = timeStamp
            // warning: we should not remove it if it was already removed
            val newSize = removeFromSet(varId, tau(varId))
            //val newSize1 = removeFromSet(varId, tau(varId))
            if (newSize == 0) removeFromSSup(j)
          }

        }
      }
    }


    ///////
    // V1
    ///////
    /*
    var i = nActiveTuples
    while (i > 0 && sSupSize != 0) {
      i -= 1
      val tau: Array[Int] = table(activeTuples(i)) // the tuple
      val isInvalid = isInvalidTuple(tau)
      if (isInvalid) deactivateTuple(i)
      else {
        // tuple i is thus valid, we need to check every variable
        // for which at least one value has not a support yet (the ones in sSup)
        var j = sSupSize
        while (j > 0) {
          j -= 1
          val varId = sSup(j)
          // remove value tau(varId) from the value to be removed
          val newSize = removeFromSet(varId, tau(varId))
          if (newSize == 0) {
            removeFromSSup(j)
          }
        }
      }
    }
    // not in Christophe paper, Guillaume Perez improvement

    while (i > 0) {
      i -= 1
      val tau = table(activeTuples(i))
      if (isInvalidTuple(tau)) deactivateTuple(i)
    }*/

    // Do the actual filtering of the domains
    i = sSupSize
    while (i > 0) {
      i -= 1
      val varId = sSup(i)
      val values = toRemoveValues(varId)
      val nValues = sizes(varId)
      val variable = variables(varId)
      val varSize = variable.size
      if (nValues > 0) {
        if (nValues == varSize) {
          // all the values should be removed, fail immediately
          return Failure
        }
        else if (nValues == varSize-1) {
          // If all but one must be removed, do a, assign instead of one by one removal.
          // To retrieve the only value to keep => the one at the last position of sparse-set
          variable.assign(values(varSize-1))
          //lastSize(varId).setValue(varSize)
          lastSize(varId).setValue(1)
        }
        else {
          var i = nValues
          while (i > 0) {
            i -= 1
            val value = values(i)
            variable.removeValue(value)
          }
          //lastSize(varId).setValue(varSize)
          lastSize(varId).setValue(varSize-nValues)
        }
      }



    }

    // Trail only if no Failure
    nActiveTuplesRev.value = nActiveTuples

    return Suspend
  }


  def updateSet(varId: Int): Unit = {
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

  def removeFromSet(varId: Int, val1: Int): Int = {
    val positions = toRemovePositions(varId)
    val offset = offsets(varId)
    val pos1 = positions(val1 - offset)
    val size = sizes(varId)

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

  def isInvalidTuple(tuple: Array[Int]): Boolean = {
    var i = sValSize
    while (i > 0) {
      i -= 1
      val varId = sVal(i)
      //val k = !variables(varId).hasValue(tuple(varId))
      if (!variables(varId).hasValue(tuple(varId))) return true
    }
    false
  }

  def removeFromSSup(id: Int): Unit = {
    val tmp = sSup(id)
    sSupSize -= 1
    sSup(id) = sSup(sSupSize)
    sSup(sSupSize) = tmp
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