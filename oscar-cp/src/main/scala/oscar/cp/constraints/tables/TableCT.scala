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

package oscar.cp.constraints.tables


import oscar.algo.reversible.{ReversibleSparseBitSet, ReversibleInt, TrailEntry}
import oscar.cp.core.CPOutcome._
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.{CPOutcome, CPPropagStrength, CPStore, Constraint}

import scala.collection.mutable.ArrayBuffer

/**
 * Implementation of the Compact Table algorithm (CT) for the table constraint.
 * @param X the variables restricted by the constraint.
 * @param table the list of tuples composing the table.
 * @author Pierre Schaus pschaus@gmail.com
 * @author Jordan Demeulenaere j.demeulenaere1@gmail.com
 */
final class TableCT(X: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(X(0).store, "TableCT") {

  /* Setting idempotency & lower priority for propagate() */
  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1

  /* Basic information */
  private[this] val arity = X.length
  private[this] val nbTuples = table.length
  private[this] val spans = Array.tabulate(arity)(i => X(i).max - X(i).min + 1)
  private[this] val originalMins = Array.tabulate(arity)(i => X(i).min)
  private[this] val store = X(0).store
  private[this] val maxDomain = X.map(_.size).max
  private[this] val domainArray = new Array[Int](maxDomain)
  private[this] var domainArraySize = 0


  private[this] val validTuples: ReversibleSparseBitSet = new ReversibleSparseBitSet(s,table.size,0 until table.size)
  private[this] val variableValueSupports = Array.tabulate(arity)(i => new Array[validTuples.BitSet](spans(i)))
  private[this] var needPropagate = false
  private[this] val deltas: Array[DeltaIntVar] = new Array[DeltaIntVar](arity)


  override def setup(l: CPPropagStrength): CPOutcome = {

    /* Retrieve the current valid tuples */
    val valids = collectValidTuples()

    if (valids.isEmpty) return Failure

    /* Remove non valide- tuples */
    validTuples.collect(new validTuples.BitSet(valids))
    validTuples.intersectCollected()

    /* Compute Supports = Compute for each for each variable/value pair the supported tuples,
       Remove values not supported by any tuple */
    if (computeSupportsAndInitialFiltering(valids) == Failure) {
      return Failure
    }

    /* Call propagate() when domains change */
    var i = 0
    while (i < arity) {
      val x = X(i)
      deltas(i) = x.callPropagateOnChangesWithDelta(this)
      i += 1
    }

    Suspend
  }

  private[this] def showTable(): Unit = {
    table.foreach { t =>
      println(t.mkString("\t"))
    }
    println("domains:"+X.mkString(","))
  }

  /**
   * Invalidates tuples by handling delta, the set of values removed from D(x) since the last call to this function.
   * @param varIndex the index of x in the array of variables.
   * @param delta the set of values removed since the last call.
   * @return the outcome i.e. Failure or Success.
   */
  @inline private def updateDelta(varIndex: Int, delta: DeltaIntVar): CPOutcome = {

    val intVar = X(varIndex)

    var changed = false
    val originalMin = originalMins(varIndex)
    val varSize = intVar.size
    val varMin = intVar.min

    validTuples.clearCollected()

    /* Update the value of validTuples by considering D(x) or delta */
    if (varSize == 1) {
      /* The variable is assigned */
      //println("validTuples empty?:"+validTuples.isEmpty())
      validTuples.collect(variableValueSupports(varIndex)(varMin-originalMin))
      changed = validTuples.intersectCollected()  //andTempMaskWithValid()
    } else {
      if (delta.size < varSize) {

        /* Use delta to update validTuples */
        domainArraySize = delta.fillArray(domainArray)
        var i = 0
        /* Collect all the removed tuples by doing or's with precomputed masks */
        while (i < domainArraySize) {
          validTuples.collect(variableValueSupports(varIndex)(domainArray(i)-originalMin))
          i += 1
        }
        /* Remove from the valid supports all the collected tuples, no longer supported */
        changed = validTuples.removeCollected()

      } else {

        /* Don't use delta = reset strategy = recompute from the domain */
        domainArraySize = intVar.fillArray(domainArray)
        var i = 0
        while (i < domainArraySize) {
          validTuples.collect(variableValueSupports(varIndex)(domainArray(i)-originalMin))
          i += 1
        }
        /* Intersect the set of valid tuples with the valid tuples collected */
        changed = validTuples.intersectCollected()

      }
    }

    /* If validTuples has changed, we need to perform a consistency check by propagate() */
    if (changed) {
      /* Failure if there are no more valid tuples */
      if (validTuples.isEmpty) return Failure
      needPropagate = true
    }

    Suspend
  }

  /**
   * Perform a consistency check : for each variable value pair (x,a), we check if a has at least one valid support.
   * Unsupported values are removed.
   * @return the outcome i.e. Failure or Success.
   */
  override def propagate(): CPOutcome = {

    needPropagate = false
    var i = 0
    var nChanged = 0
    var changedVarIdx = 0

    while (i < arity) {
      if (deltas(i).size > 0) {
        nChanged += 1
        changedVarIdx = i
        if (updateDelta(i,deltas(i)) == Failure) {
          return Failure
        }
      }
      i += 1
    }
    // No need for the check if validTuples has not changed
    if (!needPropagate) return Suspend

    var varIndex = 0
    while (varIndex < arity) {
      // no need to check a variable if it was the only one modified
      if (nChanged > 1 || changedVarIdx != varIndex) {
        domainArraySize = X(varIndex).fillArray(domainArray)
        var i = 0
        var value = 0
        while (i < domainArraySize) {
          value = domainArray(i)
          if (!validTuples.intersect(variableValueSupports(varIndex)(value - originalMins(varIndex)))) {
            if (X(varIndex).removeValue(value) == Failure) {
              return Failure
            }
          }
          i += 1
        }
      }
      varIndex += 1
    }
    Suspend
  }


  /* ----- Functions used during the setup of the constraint ----- */

  /**
   * Retrieve the valid tuples from the table and store their index in validTuplesBuffer.
   * @return Failure if there is no valid tuples, Suspend otherwise.
   */
  @inline private def collectValidTuples(): ArrayBuffer[Int] = {

    val validTuplesBuffer = ArrayBuffer[Int]()

    var tupleIndex = 0
    while (tupleIndex < nbTuples) {
      if (isTupleValid(tupleIndex)) {
        validTuplesBuffer += tupleIndex
      }
      tupleIndex += 1
    }

    return validTuplesBuffer
  }

  /**
   * Check if a tuple is valid.
   * @param tupleIndex the index of the tuple in the table.
   * @return true if the tuple is valid, false otherwise.
   */
  @inline private def isTupleValid(tupleIndex: Int): Boolean = {
    var varIndex = 0
    while (varIndex < arity) {
      if (!X(varIndex).hasValue(table(tupleIndex)(varIndex))) {
        return false
      }
      varIndex += 1
    }
    true
  }

  /**
   * Compute the mask for each variable value pair (x,a).
   */
  @inline private def computeSupportsAndInitialFiltering(valids: ArrayBuffer[Int]): CPOutcome = {

    val varValueSupports = Array.tabulate(X.size)(i => Array.tabulate(spans(i))(v => new ArrayBuffer[Int]()))

    /* Collect the supports */
    var validIndex = 0
    while (validIndex < valids.length) {
      val tupleIndex = valids(validIndex)
      var varIndex = 0
      while (varIndex < arity) {
        val value = table(tupleIndex)(varIndex)
        val valueIndex = value - originalMins(varIndex)
        varValueSupports(varIndex)(valueIndex) += tupleIndex
        varIndex += 1
      }
      validIndex += 1
    }

    /* Create the final support bitSets and remove any value that is not supported */
    for (varIndex <- 0 until variableValueSupports.size;
         valueIndex <- 0 until variableValueSupports(varIndex).size)
    {
      if (varValueSupports(varIndex)(valueIndex).size > 0) {
        variableValueSupports(varIndex)(valueIndex) = new validTuples.BitSet(varValueSupports(varIndex)(valueIndex))
      } else {
        /* This variable-value does not have any support, it can be removed */
        if (X(varIndex).removeValue(valueIndex+originalMins(varIndex)) == Failure) {
          return Failure
        }
      }
    }
    Suspend
  }
}