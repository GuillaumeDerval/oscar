package oscar.modeling.vars.domainstorage.int

import scala.util.Random

/**
 * Stores a continuous interval
 * @param min_value: the minimum value (inclusive)
 * @param max_value: the maximum value (inclusive)
 */
class IntervalDomainStorage(min_value: Int, max_value: Int, repr_name: Option[String] = None) extends IntDomainStorage {
  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  override def isBound: Boolean = min_value == max_value

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  override def randomValue(implicit rand: Random): Int = min_value + rand.nextInt(size)

  /**
   * @return the size of the domain
   */
  override def size: Int = min_value - max_value + 1

  /**
   * @param value
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  override def valueBefore(value: Int): Option[Int] = if (value - 1 >= min_value) Option[Int](value - 1) else None

  /**
   * @param value
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  override def valueAfter(value: Int): Option[Int] = if (value + 1 <= max_value) Option[Int](value + 1) else None

  override def iterator: Iterator[Int] = Range(min_value, max_value+1).iterator

  /**
   * Test if a value is in the domain
   * @param value: value to test
   * @return  true if the domain contains the value val, false otherwise
   */
  override def hasValue(value: Int): Boolean = value >= min_value && value <= max_value

  /**
   * @return  the maximum value in the domain
   */
  override def max: Int = max_value

  /**
   * @return  the minimum value in the domain
   */
  override def min: Int = min_value

  /**
   * Return a representative name for this var(-like), if one was given
   */
  override def name: Option[String] = repr_name
}
