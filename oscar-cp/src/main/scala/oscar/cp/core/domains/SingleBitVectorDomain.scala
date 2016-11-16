package oscar.cp.core.domains

import oscar.algo.reversible.{ReversibleContext, ReversibleInt}

import scala.util.Random
import oscar.algo.search.Outcome._
import java.lang.Integer.bitCount

import oscar.algo.search.Outcome

/** 
 *  An integer domain based on a bit vector of 32 bits. 
 *  @author Renaud Hartert 
 */
class SingleBitVectorDomain(override val context: ReversibleContext, val minValue: Int, val maxValue: Int) extends IntDomain {

  // Initial size of the domain
  private val maxBits = maxValue - minValue + 1

  // Consistency check
  require(maxBits <= 32, "the set cannot contain more than 32 elements")

  // Domain representation
  private val bits = new ReversibleInt(context, (1 << maxBits) - 1)
  private val minId = new ReversibleInt(context, 0)
  private val maxId = new ReversibleInt(context, maxBits - 1)

  override def size: Int = bitCount(bits.value)

  override def isEmpty: Boolean = bits.value == 0

  override def isBound: Boolean = bitCount(bits.value) == 1

  override def min: Int = {
    if (bits.value == 0) throw new NoSuchElementException("empty")
    else {
      updateMinId()
      minValue + minId.value
    }
  }

  override def max: Int = {
    if (bits.value == 0) throw new NoSuchElementException("empty")
    else {
      updateMaxId()
      minValue + maxId.value
    }
  }

  override def randomValue(rand: Random): Int = {
    val n = size
    val r = rand.nextInt(n) + 1
    val minVal = min // Lazy update
    if (r == 1) minVal 
    else if (r == n) max
    else {
      val domain = bits.value
      var counter = 1
      var id = minId.value + 1 // up to date
      while (counter < r) {
        val bit = (1 << id)
        if ((domain & bit) == bit) counter += 1
        id += 1
      }
      minValue + id
    }
  }

  def hasValue(value: Int): Boolean = {
    if (value < minValue) false
    else if (value > maxValue) false
    else {
      val bit = 1 << (value - minValue)
      (bits.value & bit) == bit
    }
  }

  def removeValue(value: Int): Outcome = {
    if (value < minValue) Suspend
    else if (value > maxValue) Suspend
    else {
      val bit = 1 << (value - minValue)
      val domain = bits.value
      // Value is not in the domain
      if ((domain & bit) == 0) Suspend
      // The domain becomes empty
      else {
        val newDomain = domain ^ bit
        bits.value = newDomain
        if (newDomain == 0) Failure
        else Suspend
      }
    }
  }

  def assign(value: Int): Outcome = {
    if (value < minValue || value > maxValue) {
      bits.value = 0
      Failure
    }
    else {
      val id = (value - minValue)
      val bit = 1 << id
      if ((bits.value & bit) == 0) {
        bits.value = 0
        Failure
      } else {
        bits.value = bit
        Suspend
      }
    }
  }

  // Loops forever if the domain is empty
  @inline private def updateMinId(): Unit = {
    var id = minId.value
    val domain = bits.value
    while ((domain & (1 << id)) == 0) id += 1
    minId.value = id
  }

  // Loops forever if the domain is empty
  @inline private def updateMaxId(): Unit = {
    var id = maxId.value
    val domain = bits.value
    while ((domain & (1 << id)) == 0) id -= 1
    maxId.value = id
  }

  def updateMin(value: Int): Outcome = {
    if (value < minValue) Suspend
    else if (value > maxValue) {
      bits.value = 0
      Failure
    }
    else {
      val id = value - minValue
      updateMinId()
      val minBitId = minId.value
      if (id <= minBitId) Suspend
      else {
        updateMaxId()
        val maxBitId = maxId.value
        if (id == maxBitId) assign(value)
        else if (id > maxBitId) {
          bits.value = 0
          Failure
        } else {
          var i = minBitId
          var domain = bits.value
          while (i < id) {
            val bit = (1 << i)
            if ((domain & bit) == bit) {
              domain = domain ^ bit
            }
            i += 1
          }
          bits.value = domain
          minId.value = id
          Suspend
        }
      }
    }
  }

  def updateMax(value: Int): Outcome = {
    if (value < minValue) {
      bits.value = 0
      Failure
    }
    else if (value > maxValue) Suspend
    else {
      val id = value - minValue
      updateMaxId()
      val maxBitId = maxId.value
      if (id >= maxBitId) Suspend
      else {
        updateMinId()
        val minBitId = minId.value
        if (id == minBitId) assign(value)
        else if (id < minBitId) {
          bits.value = 0
          Failure
        } else {
          var i = maxBitId
          var domain = bits.value
          while (i > id) {
            val bit = (1 << i)
            if ((domain & bit) == bit) {
              domain = domain ^ bit
            }
            i -= 1
          }
          bits.value = domain
          maxId.value = id
          Suspend
        }
      }
    }
  }

  def nextValue(value: Int): Int = {
    if (value > maxValue) value - 1
    else {
      val id = math.max(0, value - minValue)
      val bit = 1 << id
      val domain = bits.value
      if ((domain & bit) == bit) minValue + id
      else {
        updateMaxId()
        updateMinId()
        val max = maxId.value
        val min = minId.value
        if (id > max) value - 1
        else if (id < min) min + minValue
        else {
          var i = id + 1
          while ((domain & (1 << i)) == 0) i += 1
          i + minValue
        }
      }
    }
  }

  def prevValue(value: Int): Int = {
    if (value < minValue) value + 1
    else {
    val id = math.min(maxValue, value - minValue)
    val bit = 1 << id
    val domain = bits.value
    if ((domain & bit) == bit) minValue + id
    else {
      updateMaxId()
      updateMinId()
      val max = maxId.value
      val min = minId.value
      if (id < min) value + 1
      else if (id > max) max + minValue
      else {
        var i = id - 1
        while ((domain & (1 << i)) == 0) i -= 1
        i + minValue
      }}
    }
  }

  override def iterator: Iterator[Int] = {
    updateMinId()
    updateMaxId()
    new Iterator[Int] {
      private var id = minId.value
      private val max = maxId.value
      private val domain = bits.value
      def next(): Int = {
        val value = minValue + id
        searchNext()
        value
      }
      def hasNext(): Boolean = id <= max
      private def searchNext(): Unit = {
        id += 1
        while (id <= max && ((domain & (1 << id)) == 0)) {
          id += 1
        }
      }
    }
  }

  // Not implemented
  def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = ???

  override def toString: String = {
    if (isEmpty) "phi"
    else "{" + this.mkString(", ") + "}"
  }
}