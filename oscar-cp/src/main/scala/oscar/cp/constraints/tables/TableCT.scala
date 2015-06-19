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

package oscar.cp.constraints.tables

import oscar.algo.reversible.{ ReversibleInt, ReversibleBoolean, TrailEntry }
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.{ Constraint, CPStore, CPOutcome, CPPropagStrength }
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.CPOutcome._
import scala.collection.mutable.ArrayBuffer
import oscar.cp.core.delta.SnapshotIntVar

/**
 * Implementation of the Compact Table algorithm (CT) for the table constraint.
 * @param X the variables restricted by the constraint.
 * @param table the list of tuples composing the table.
 * @author Jordan Demeulenaere j.demeulenaere1@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */

/* Trailable entry to restore the value of the ith Long of the valid tuples */
final class TableCTLongTrailEntry(table: TableCT, i: Int, value: Long) extends TrailEntry {
  @inline override def restore(): Unit = table.restore(i, value)
}

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

  /* Bitsets and other arrays */
  private[this] var nbLongs = 0
  private[this] val validTuplesBuffer = ArrayBuffer[Int]()
  private[this] var validTuples: Array[Long] = null
  private[this] val masks = Array.tabulate(arity)(i => new Array[Array[Long]](spans(i)))
  private[this] var lastMagics: Array[Long] = null
  private[this] var tempMask: Array[Long] = null

  /* We keep the valid longs in a sparse set */
  private[this] var validLongs: Array[Int] = null
  private[this] val nValidLongsRev = new ReversibleInt(s, 0)
  private[this] var nValidLongs: Int = 0

  /* Structures for the improvements */
  private[this] var touchedVar = -1
  private[this] val lastSupports = Array.tabulate(arity)(i => new Array[Int](spans(i)))
  private[this] val lastSizes = Array.fill(arity)(new ReversibleInt(store, 0))
  private[this] val needPropagate = new ReversibleBoolean(store, false)

  override def setup(l: CPPropagStrength): CPOutcome = {

    /* Retrieve the current valid tuples */
    if (fillValidTuples() == Failure) return Failure

    /* Compute the masks for each (x,a) pair */
    fillMasks()

    /* Remove values not supported by any tuple */
    if (removeUnsupportedValues() == Failure) {
      return Failure
    }

    /* Call propagate() and update(x, delta) when domains change */
    var i = 0
    while (i < arity) {
      val x = X(i)
      val varIndex = i
      x.filterWhenDomainChangesWithDelta(idempotent = true, CPStore.MaxPriorityL2)(delta => updateDelta(x, varIndex, delta))
      x.callPropagateWhenDomainChanges(this)
      lastSizes(i).setValue(x.size)
      i += 1
    }

    Suspend
  }

  /**
   * Invalidates tuples by handling delta, the set of values removed from D(x) since the last call to this function.
   * @param intVar the CPIntVar associated to x.
   * @param varIndex the index of x in the array of variables.
   * @param delta the set of values removed since the last call.
   * @return the outcome i.e. Failure or Success.
   */
  @inline private def updateDelta(intVar: CPIntVar, varIndex: Int, delta: SnapshotIntVar): CPOutcome = {

    /* No need to update validTuples if there was no modification since last propagate() */
    if (intVar.size == lastSizes(varIndex).value) return Suspend

    // Cache reversible values
    nValidLongs = nValidLongsRev.value

    var changed = false
    val originalMin = originalMins(varIndex)
    val varSize = intVar.size
    val varMin = intVar.min

    /* Update the value of validTuples by considering D(x) or delta */
    lastSizes(varIndex).setValue(varSize)
    if (varSize == 1) {
      /* The variable is assigned */
      setTempMask(varIndex, varMin - originalMin)
      changed = andTempMaskWithValid()
    } else {
      val varMax = intVar.max
      if (varSize == 2) {
        /* The variable has only two values */
        setTempMask(varIndex, varMin - originalMin)
        orTempMask(varIndex, varMax - originalMin)
        changed = andTempMaskWithValid()
      } else {
        clearTempMask()
        if (delta.size < varSize) {
          /* Use delta to update validTuples */
          domainArraySize = delta.fillArray(domainArray)
          var i = 0
          while (i < domainArraySize) {
            orTempMask(varIndex, domainArray(i) - originalMin)
            i += 1
          }
          changed = substractTempMaskFromValid()
        } else {
          /* Use domain to update validTuples */
          if (varMax - varMin + 1 == varSize) {
            /* The domain is an interval */
            var value = varMin
            while (value <= varMax) {
              orTempMask(varIndex, value - originalMin)
              value += 1
            }
          } else {
            /* The domain is sparse */
            domainArraySize = intVar.fillArray(domainArray)
            var i = 0
            while (i < domainArraySize) {
              orTempMask(varIndex, domainArray(i) - originalMin)
              i += 1
            }
          }
          changed = andTempMaskWithValid()
        }
      }
    }

    /* If validTuples has changed, we need to perform a consistency check by propagate() */
    if (changed) {
      /* Failure if there are no more valid tuples */
      if (nValidLongs == 0) return Failure

      /* We check if x was the only modified variable since last propagate() */
      if (touchedVar == -1 || touchedVar == varIndex) touchedVar = varIndex
      else touchedVar = -2
      
      needPropagate.setTrue()
    }

    // Trail reversibles
    nValidLongsRev.value = nValidLongs

    Suspend
  }

  /**
   * Perform a consistency check : for each variable value pair (x,a), we check if a has at least one valid support.
   * Unsupported values are removed.
   * @return the outcome i.e. Failure or Success.
   */
  override def propagate(): CPOutcome = {

    // No need for the check if validTuples has not changed 
    if (!needPropagate.value) return Suspend

    // Cache reversible values
    nValidLongs = nValidLongsRev.value

    if (touchedVar == -2) {
      touchedVar = -1
    }

    /* For each variable value (x,a), check if a is supported. Unsupported values are removed from their respective
     * domains */
    var varIndex = 0
    while (varIndex < arity) {
      /* No need to check the values of a variable if it was the only modified since last check */
      if (touchedVar == varIndex) {
        touchedVar = -1
      } else {
        val intVar = X(varIndex)
        val originalMin = originalMins(varIndex)
        val varSize = intVar.size

        if (varSize == 1) {
          if (!supported(varIndex, intVar.min - originalMin)) {
            return Failure
          }
        } else if (varSize == 2) {
          val varMin = intVar.min
          val varMax = intVar.max
          val minSupported = supported(varIndex, varMin - originalMin)
          val maxSupported = supported(varIndex, varMax - originalMin)
          if (!minSupported) {
            if (!maxSupported) {
              return Failure
            } else {
              intVar.assign(varMax)
              lastSizes(varIndex).setValue(1)
            }
          } else if (!maxSupported) {
            intVar.assign(varMin)
            lastSizes(varIndex).setValue(1)
          }
        } else {
          val varMin = intVar.min
          val varMax = intVar.max

          /* Domain of the variable is an interval */
          if (varMax - varMin + 1 == varSize) {
            var value = varMin
            while (value <= varMax) {
              if (!supported(varIndex, value - originalMin)) {
                if (intVar.removeValue(value) == Failure) {
                  return Failure
                }
              }
              value += 1
            }
          } else { /* Domain is sparse */
            domainArraySize = intVar.fillArray(domainArray)
            var i = 0
            var value = 0
            while (i < domainArraySize) {
              value = domainArray(i)
              if (!supported(varIndex, value - originalMin)) {
                if (intVar.removeValue(value) == Failure) {
                  return Failure
                }
              }
              i += 1
            }
          }

          lastSizes(varIndex).setValue(intVar.size)
        }

      }
      varIndex += 1
    }

    needPropagate.setFalse()

    // Trail reversibles
    nValidLongsRev.value = nValidLongs

    Suspend
  }

  /* ----- Functions used during propagation ----- */

  /**
   * Check wether a variable value (x,a) is supported by the current validTuples.
   * @param varIndex the index of the variable.
   * @param valueIndex the index of the value (i.e. value - originalMin(x)).
   * @return true if (x,a) is supported, false otherwise.
   */
  @inline private final def supported(varIndex: Int, valueIndex: Int): Boolean = {
    /* We check if the last support is still a support */
    val mask = masks(varIndex)(valueIndex)
    val support = lastSupports(varIndex)(valueIndex)
    if ((mask(support) & validTuples(support)) != 0) {
      return true
    }

    /* We check the equations
     *         mask(x,a) & validTuples != 0
     * only for the relevant parts of the bitsets. */
    var i = nValidLongs
    while (i > 0) {
      i -= 1
      val offset = validLongs(i)
      if ((mask(offset) & validTuples(offset)) != 0) {
        /* We found a support and we store the index of the Long where the support is */
        lastSupports(varIndex)(valueIndex) = offset
        return true
      }
    }

    false
  }

  /**
   * Set the bitset tempMask to be the same as mask(x,a).
   * @param varIndex the index of x.
   * @param valueIndex the index of a (i.e. a - originalMin(x)).
   */
  @inline private def setTempMask(varIndex: Int, valueIndex: Int): Unit = {
    var i = nbLongs
    while (i > 0) {
      i -= 1
      tempMask(i) = masks(varIndex)(valueIndex)(i)
    }
  }

  /**
   * Clear the tempMask.
   */
  @inline private def clearTempMask(): Unit = {
    var i = nbLongs
    while (i > 0) {
      i -= 1
      tempMask(i) = 0L
    }
  }

  /**
   * Apply the OR bitwise operation between tempMask and mask(x,a), and store the result in tempMask :
   *      tempMask = tempMask |= mask(x,a)
   * @param varIndex the index of x.
   * @param valueIndex the index of a (i.e. a - originalMin(x)).
   */
  @inline private def orTempMask(varIndex: Int, valueIndex: Int): Unit = {
    val mask = masks(varIndex)(valueIndex)
    var i = nValidLongs
    while (i > 0) {
      i -= 1
      val offset = validLongs(i)
      tempMask(offset) |= mask(offset)
    }
  }

  /**
   * Apply the AND bitwise operation between validTuples and tempmask, and store the result in validTuples :
   *      validTuples = validTuples & tempMask
   * @return true if validTuples has changed, false otherwise.
   */
  @inline private def andTempMaskWithValid(): Boolean = {
    var changed = false
    var i = nValidLongs
    while (i > 0) {
      i -= 1
      val offset = validLongs(i)
      val tempMaskOffset = tempMask(offset)
      if ((~tempMaskOffset & validTuples(offset)) != 0) {
        andValidTuples(i, offset, tempMaskOffset)
        changed = true
      }
    }
    changed
  }

  /**
   * Set to 0 each bit in validTuples that are set in tempMask, i.e. :
   *      validTuples = validTuples & (~tempMask)
   * @return true if validTuples has changed, false otherwise.
   */
  @inline private def substractTempMaskFromValid(): Boolean = {
    var changed = false
    var i = nValidLongs
    while (i > 0) {
      i -= 1
      val offset = validLongs(i)
      val tempMaskOffset = tempMask(offset)
      if ((tempMaskOffset & validTuples(offset)) != 0) {
        andValidTuples(i, offset, ~tempMaskOffset)
        changed = true
      }
    }
    changed
  }

  /**
   * Apply the AND bitwise operation with validTuples[offset] and another Long :
   *      validTuples[offset] = validTuples[offset] & mask
   * Trail the value of validTuples[offset] before if it is the first time that it is changed in this search node.
   * @param offset the index of the Long in validTuples to change.
   * @param mask the mask to apply.
   */
  @inline private def andValidTuples(position: Int, offset: Int, mask: Long): Unit = {
    
    val storeMagic = store.magic
    if (lastMagics(offset) != storeMagic) {
      lastMagics(offset) = storeMagic
      trail(offset)
    }
    
    val newLong = validTuples(offset) & mask
    validTuples(offset) = newLong

    // Remove the long from the set if
    if (newLong == 0) {
      nValidLongs -= 1
      validLongs(position) = validLongs(nValidLongs)
      validLongs(nValidLongs) = offset
    }
  }

  /**
   * Trail the value of validTuples[offset].
   * @param offset the index of the Long to trail.
   */
  @inline private def trail(offset: Int): Unit = {
    val trailEntry = new TableCTLongTrailEntry(this, offset, validTuples(offset))
    store.trail(trailEntry)
  }

  /**
   * Restore validTuples[offset] to an old value.
   * @param offset the index of the Long to restore.
   * @param value the value to restore.
   */
  @inline final def restore(offset: Int, value: Long): Unit = validTuples(offset) = value

  /* ----- Functions used during the setup of the constraint ----- */

  /* Bits operations */
  @inline private def oneBitLong(pos: Int): Long = 1L << pos
  @inline private def bitLength(size: Int): Int = (size + 63) >>> 6 // = pos / 64 + 1
  @inline private def bitOffset(pos: Int): Int = pos >>> 6 // = pos / 64
  @inline private def bitPos(pos: Int): Int = pos & 63 // = pos % 63
  @inline private def setBit(bitset: Array[Long], pos: Int): Unit = bitset(bitOffset(pos)) |= oneBitLong(bitPos(pos))

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
   * Retrieve the valid tuples from the table and store their index in validTuplesBuffer.
   * @return Failure if there is no valid tuples, Suspend otherwise.
   */
  @inline private def fillValidTuples(): CPOutcome = {
    validTuplesBuffer.clear()
    var tupleIndex = 0
    while (tupleIndex < nbTuples) {
      if (isTupleValid(tupleIndex)) {
        validTuplesBuffer += tupleIndex
      }
      tupleIndex += 1
    }

    if (validTuplesBuffer.isEmpty) {
      return Failure
    }

    /* Compute number of Long in a bitset */
    nbLongs = bitLength(validTuplesBuffer.length)

    Suspend
  }

  /**
   * Compute the mask for each variable value pair (x,a).
   */
  @inline private def fillMasks(): Unit = {
    tempMask = Array.fill(nbLongs)(0L)
    validTuples = Array.fill(nbLongs)(0L)
    lastMagics = Array.fill(nbLongs)(-1L)
    validLongs = Array.tabulate(nbLongs)(i => i)
    nValidLongsRev.value = nbLongs

    var validIndex = 0
    while (validIndex < validTuplesBuffer.length) {
      setBit(validTuples, validIndex)

      val tupleIndex = validTuplesBuffer(validIndex)
      var varIndex = 0
      while (varIndex < arity) {
        val value = table(tupleIndex)(varIndex)
        val valueIndex = value - originalMins(varIndex)
        var mask = masks(varIndex)(valueIndex)

        if (mask == null) {
          mask = Array.fill(nbLongs)(0L)
          masks(varIndex)(valueIndex) = mask
        }
        setBit(mask, validIndex)

        varIndex += 1
      }

      validIndex += 1
    }

    validTuplesBuffer.clear()
  }

  /**
   * Remove values not supported by any tuple.
   * @return the outcome i.e. Failure or Success.
   */
  @inline private def removeUnsupportedValues(): CPOutcome = {
    var varIndex = 0
    while (varIndex < arity) {
      val intVar = X(varIndex)
      domainArraySize = intVar.fillArray(domainArray)
      var i = 0
      while (i < domainArraySize) {
        val value = domainArray(i)
        val valueIndex = value - originalMins(varIndex)
        if (masks(varIndex)(valueIndex) == null) {
          if (intVar.removeValue(value) == Failure) {
            return Failure
          }
        }
        i += 1
      }
      varIndex += 1
    }

    Suspend
  }

}