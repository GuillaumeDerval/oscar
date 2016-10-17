package oscar.modeling.vars.domainstorage.int

import scala.util.Random

/**
 * A proxy to other IntDomainStorage. Allow to change representation when needed.
 * Note: this class supports IntervalDomainStorage, SetDomainStorage, and SingletonDomainStorage
 * @param content: another domain to proxy
 */
class AdaptableIntDomainStorage(val content: IntDomainStorage) extends IntDomainStorage {
  //Check type
  if (!content.isInstanceOf[IntervalDomainStorage] && !content.isInstanceOf[SetDomainStorage] &&
    !content.isInstanceOf[SingletonDomainStorage])
    throw new IllegalArgumentException("Invalid type")

  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  override def isBound: Boolean = content.isBound

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  override def randomValue(implicit rand: Random): Int = content.randomValue(rand)

  /**
   * @return  the maximum value in the domain
   */
  override def max: Int = content.max

  /**
   * @param value
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  override def valueBefore(value: Int): Option[Int] = content.valueBefore(value)

  /**
   * @return  the minimum value in the domain
   */
  override def min: Int = content.min

  /**
   * @param value
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  override def valueAfter(value: Int): Option[Int] = content.valueAfter(value)

  override def iterator: Iterator[Int] = content.iterator

  /**
   * Test if a value is in the domain
   * @param value: value to test
   * @return  true if the domain contains the value val, false otherwise
   */
  override def hasValue(value: Int): Boolean = content.hasValue(value)

  /**
   * Return a representative name for this var(-like), if one was given
   */
  override def name: Option[String] = content.name
}
