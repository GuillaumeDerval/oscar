package oscar.modeling.vars.domainstorage.int

import oscar.modeling.vars.IntVarImplem
import oscar.modeling.vars.domainstorage.DomainStorage

import scala.collection.immutable.SortedSet

trait IntDomainStorage extends DomainStorage with IntVarImplem {}

object IntDomainStorage {
  /**
   * Create a new IntDomainStorage from a Set of values
   * @param s
   */
  def apply(s: Set[Int], name: Option[String]) = new AdaptableIntDomainStorage(new SetDomainStorage(SortedSet(s.toList: _*), name))
  def apply(s: Set[Int]) = new AdaptableIntDomainStorage(new SetDomainStorage(SortedSet(s.toList: _*)))
  /**
   * Create a new IntDomainStorage from a Set of values
   * @param s
   */
  def apply(s: SortedSet[Int], name: Option[String]) = new AdaptableIntDomainStorage(new SetDomainStorage(s, name))
  def apply(s: SortedSet[Int]) = new AdaptableIntDomainStorage(new SetDomainStorage(s))

  /**
   * Create a new IntDomainStorage containing only a single value
   * @param v
   */
  def apply(v: Int, name: Option[String]) = new AdaptableIntDomainStorage(new SingletonDomainStorage(v, name))
  def apply(v: Int) = new AdaptableIntDomainStorage(new SingletonDomainStorage(v))

  /**
   * Create a new IntDomainStorage, with domain from min to max, inclusive
   * @param min
   * @param max
   */
  def apply(min: Int, max: Int, name: Option[String]) = new AdaptableIntDomainStorage(new IntervalDomainStorage(min, max, name))
  def apply(min: Int, max: Int) = new AdaptableIntDomainStorage(new IntervalDomainStorage(min, max))
}