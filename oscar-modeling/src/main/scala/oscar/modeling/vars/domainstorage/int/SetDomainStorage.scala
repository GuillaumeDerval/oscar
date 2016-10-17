package oscar.modeling.vars.domainstorage.int

import scala.collection.immutable.SortedSet
import scala.util.Random

/**
 * Stores a set of int
 * @param content: the domain
 */
class SetDomainStorage(val content: SortedSet[Int], repr_name: Option[String] = None) extends IntDomainStorage {
//  def this(content: Set[Int], repr_name: Option[String]) = this(SortedSet(content.toList: _*), repr_name)
//  def this(content: Set[Int]) = this(SortedSet(content.toList: _*))

  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  override def isBound: Boolean = content.size == 1

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  override def randomValue(implicit rand: Random): Int = content.toVector(rand.nextInt(size))

  /**
   * @return the size of the domain
   */
  override def size: Int = content.size

  /**
   * @return  the maximum value in the domain
   */
  override def max: Int = content.max

  /**
   * @param value
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  override def valueBefore(value: Int): Option[Int] = content.to(value - 1).lastOption

  /**
   * @return  the minimum value in the domain
   */
  override def min: Int = content.min

  /**
   * @param value
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  override def valueAfter(value: Int): Option[Int] = content.from(value + 1).headOption

  override def iterator: Iterator[Int] = content.iterator

  /**
   * Test if a value is in the domain
   * @param value: value to test
   * @return  true if the domain contains the value val, false otherwise
   */
  override def hasValue(value: Int): Boolean = content.contains(value)

  /**
   * Return a representative name for this var(-like), if one was given
   */
  override def name: Option[String] = repr_name
}