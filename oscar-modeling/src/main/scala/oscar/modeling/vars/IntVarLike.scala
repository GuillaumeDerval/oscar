package oscar.modeling.vars

import scala.util.Random

/**
 * A trait that all objects that behave like an IntVar should implement
 */
trait IntVarLike extends Iterable[Int] {
  def isContinuous: Boolean = size == (max - min + 1)

  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  def isBound: Boolean

  /**
   * @param v: value to test
   * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
   */
  def isBoundTo(v: Int): Boolean = isBound && hasValue(v)

  /**
   * Test if a value is in the domain
   * @param value: value to test
   * @return  true if the domain contains the value val, false otherwise
   */
  def hasValue(value: Int): Boolean

  /**
   * @param value
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  def valueAfter(value: Int): Option[Int]

  /**
   * @param value
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  def valueBefore(value: Int): Option[Int]

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  def randomValue(implicit rand: Random): Int

  /**
   * @return the size of the domain
   */
  def size: Int

  /**
   * @return the size of the domain
   */
  def getSize = size

  /**
   * @return true if domain is empty, false else
   */
  override def isEmpty: Boolean = size == 0

  /**
   * @return  the minimum value in the domain
   */
  def min: Int

  /**
   * @return the minimum value in the domain
   */
  def getMin = min

  /**
   * @return  the maximum value in the domain
   */
  def max: Int

  /**
   * @return the maximum value in the domain
   */
  def getMax = max

  /**
   * Number of values in common in both domains
   * @param other
   * @return Number of values in common in both domains
   */
  def intersectionSize(other: IntVarImplem): Int = {
    if (other.min > max) return 0
    if (other.max < min) return 0
    var res = 0
    var v = other.min.max(min)
    while (v <= other.max.min(max)) {
      if (hasValue(v) && other.hasValue(v))
        res += 1
      v += 1
    }
    res
  }

  def iterator: Iterator[Int]

  override def foreach[@specialized(Int) U](f: Int => U): Unit = iterator.foreach(f)

  /**
   * @return an (not sorted) array representation of the domain.
   */
  def toArray: Array[Int] = iterator.toArray

  /**
   * @param array.length >= this.size
   * @return Fills the array with the domain.
   *         returns the number of values (this.size).
   *         The array is not sorted.
   */
  def fillArray(array: Array[Int]): Int = {
    val ite = iterator
    var i = 0
    while (ite.hasNext) {
      array(i) = ite.next()
      i += 1
    }
    i
  }

  /**
   * Return a representative name for this var(-like), if one was given
   */
  def getRepresentativeName: Option[String]
}