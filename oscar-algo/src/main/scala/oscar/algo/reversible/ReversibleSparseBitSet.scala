/** *****************************************************************************
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
  * *****************************************************************************/


package oscar.algo.reversible



object BitSetOp {

  def bitLength(size: Int): Int = (size + 63) >>> 6

  // = size / 64 + 1
  def oneBitLong(pos: Int): Long = 1L << pos

  // = pos / 64 (64 = 2^6)
  def bitOffset(pos: Int): Int = pos >>> 6

  // = pos % 64
  def bitPos(pos: Int): Int = pos & 63

  // = pos % 63
  def setBit(bitSet: Array[Long], pos: Int): Unit = {
    bitSet(bitOffset(pos)) |= oneBitLong(bitPos(pos))
  }

}


import BitSetOp._

/* Trailable entry to restore the value of the ith Long of the valid tuples */
final class ReversibleSparseBitSetLongTrailEntry(set: ReversibleSparseBitSet, i: Int, value: Long, nNonZero: Int) extends TrailEntry {
  @inline override def restore(): Unit = set.restore(i, value,nNonZero)
}

/**
 * A reversible set with an internal bit-set representation.
 * This set can remove efficiently its elements from another bit-set
 * This set can compute efficiently its intersection with another bit-set
 * @param context
 * @param n initial values must be taken from {0,...,n-1}
 * @param initialValues the initial values contained in the set
 */
class ReversibleSparseBitSet(context: ReversibleContext, val n: Int, val initialValues: Iterable[Int]) {

  /**
   * Immutable bit-set that can be used to remove/intersect
   * with the the ReversibleSparseBitSet
   * @param values initial values, they must be in {0,...,n-1}
   */
  class BitSet(values: Iterable[Int]) {

    protected[ReversibleSparseBitSet] var lastSupport = 0

    protected[ReversibleSparseBitSet] var words: Array[Long] = Array.fill(nWords)(0L)

    assert(values.forall(v => v < n && v >= 0))

    values.foreach(v => setBit(words, v))

  }




  /* Compute number of Long in a bitset */
  private[this] var nWords = bitLength(n)

  private[this] val words: Array[Long] = Array.fill(nWords)(0L)
  private[this] val lastMagics = Array.fill(nWords)(-1L)


  private[this] var nonZeroIdx: Array[Int] = Array.tabulate(nWords)(i => i)
  private[this] var nNonZero: Int =  nWords


  private[this] val tempMask = Array.fill(nWords)(0L)

  assert(initialValues.forall(v => v < n && v >= 0))

  initialValues.foreach(v => setBit(words, v))


  // Remove the zero words from sparse set
  var i: Int = nNonZero
  while (i > 0) {
    i -= 1

    if (words(nonZeroIdx(i)) == 0L) {
      nNonZero -= 1
      nonZeroIdx(i) = nonZeroIdx(nNonZero)
      nonZeroIdx(nNonZero) = i
    }

  }

  def isEmpty() : Boolean = {
    nNonZero == 0
  }

  override def toString(): String = {
    val size = n min 64
    def format(l: Long) = String.format(s"%${size}s", java.lang.Long.toBinaryString(l)).replace(' ', '0')

    "NonZeroWords:"+nNonZero +" words:"+words.map(format(_)).mkString(" , ")
  }


  @inline final def restore(offset: Int, value: Long, nNonNull: Int): Unit = {
    words(offset) = value
    nNonZero = nNonNull
  }

  @inline private def trail(offset: Int): Unit = {
    val trailEntry = new ReversibleSparseBitSetLongTrailEntry(this, offset, words(offset),nNonZero)
    context.trail(trailEntry)
  }

  /**
   * Clear all the collected elements
   */
  def clearCollected(): Unit = {

    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      tempMask(nonZeroIdx(i)) = 0L
    }
  }

  /**
   * Add the elements in set in the set of collected elements
   * to be used with a subsequent intersectCollected() or removeCollected() operation
   * @param set
   */
  def collect(set: BitSet): Unit = {
    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      tempMask(offset) |= set.words(offset)
    }
  }

  /**
   * Change the bit set such that only the elements
   * also present in the collected set are kept
   * @return true if the set has changed, false otherwise.
   */
  def intersectCollected(): Boolean = {
    var changed = false
    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      val tempMaskOffset = tempMask(offset)
      /* They should at be at least one bit set in words(offset) */
      /* and not set in tempMaskOffset to have a change */
      if ((~tempMaskOffset & words(offset)) != 0L) {
        andWordWithMask(i, offset, tempMaskOffset)
        changed = true
      }
    }
    changed
  }

  /**
   * Change the bit set such that all the elements collected
   * are removed from the bit-set
   * @return true if the set has changed, false otherwise.
   */
  def removeCollected(): Boolean = {
    var changed = false
    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      val tempMaskOffset = tempMask(offset)
      /* There should be at least one bit in common to have a change */
      if ((tempMaskOffset & words(offset)) != 0L) {
        andWordWithMask(i, offset, ~tempMaskOffset)
        changed = true
      }
    }
    changed
  }

  @inline private def andWordWithMask(position: Int, offset: Int, mask: Long): Unit = {

    val storeMagic = context.magic
    if (lastMagics(offset) != storeMagic) {
      lastMagics(offset) = storeMagic
      trail(offset)
    }

    val newLong: Long = words(offset) & mask
    words(offset) = newLong

    /* Remove the word from the sparse set if equal to 0 */
    if (newLong == 0L) {
      nNonZero -= 1
      nonZeroIdx(position) = nonZeroIdx(nNonZero)
      nonZeroIdx(nNonZero) = offset
    }
  }

  /**
   * @param set
   * @return true if set has a non empty intersection with the bit-set
   */
  def intersect(set: BitSet): Boolean = {
    val support = set.lastSupport

    if ((words(support) & set.words(support)) != 0L) {
      return true
    }

    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if ((words(offset) & set.words(offset)) != 0L) {
        /* We found a support and we store the index of the Long where the support is */
        set.lastSupport = offset
        return true
      }
    }

    false
  }


}