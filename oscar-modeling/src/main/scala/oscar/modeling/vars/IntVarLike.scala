package oscar.modeling.vars

import scala.util.Random

/**
 * A trait that all objects that behave like an IntVar should implement
 */
trait IntVarLike extends Iterable[Int] {
  def isContinuous: Boolean

  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  def isBound: Boolean

  /**
   * @param v: value to test
   * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
   */
  def isBoundTo(v: Int): Boolean

  /**
   * Test if a value is in the domain
   * @param value: value to test
   * @return  true if the domain contains the value val, false otherwise
   */
  def hasValue(value: Int): Boolean

  /**
   * @param value
   * @return the smallest value > val in the domain
   */
  def valueAfter(value: Int): Int

  /**
   * @param value
   * @return the largest value < val in the domain
   */
  def valueBefore(value: Int): Int

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  def randomValue(rand: Random): Int

  /**
   * @return the size of the domain
   */
  def size: Int

  /**
   * @return the size of the domain
   */
  def getSize: Int

  /**
   * @return true if domain is empty, false else
   */
  def isEmpty: Boolean

  /**
   * @return  the minimum value in the domain
   */
  def min: Int

  /**
   * @return the minimum value in the domain
   */
  def getMin: Int

  /**
   * @return  the maximum value in the domain
   */
  def max: Int

  /**
   * @return the maximum value in the domain
   */
  def getMax: Int

  def iterator: Iterator[Int]

  def foreach[@specialized(Int) U](f: Int => U): Unit

  /**
   * @return an (not sorted) array representation of the domain.
   */
  def toArray: Array[Int]

  /**
   * @param array.length >= this.size
   * @return Fills the array with the domain.
   *         returns the number of values (this.size).
   *         The array is not sorted.
   */
  def fillArray(array: Array[Int]): Int

  /**
   * Return a representative name for this var(-like), if one was given
   */
  def name: String
}

trait IntVarLikeReusable extends IntVarLike {
  def isContinuous: Boolean = size == (max - min + 1)
  def isBoundTo(v: Int): Boolean = isBound && hasValue(v)
  def getSize = size
  override def isEmpty: Boolean = size == 0
  def getMin = min
  def getMax = max
  override def foreach[@specialized(Int) U](f: Int => U): Unit = iterator.foreach(f)
  def toArray: Array[Int] = iterator.toArray
  def fillArray(array: Array[Int]): Int = {
    val ite = iterator
    var i = 0
    while (ite.hasNext) {
      array(i) = ite.next()
      i += 1
    }
    i
  }
}