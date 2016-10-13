package oscar.modeling.vars.cp.int

import oscar.modeling.vars.IntVarImplem
import oscar.modeling.vars.cp.CPVar
import oscar.modeling.vars.domainstorage.int._

import scala.util.Random

abstract class CPIntVar extends CPVar with IntVarImplem {
  val realCPVar: oscar.cp.CPIntVar
}

object CPIntVar {
  def apply(notInstantied: IntDomainStorage, store: oscar.cp.CPStore): CPIntVar = new CPIntVarImplem(notInstantied, store)
}

class CPIntVarImplem(notInstantied: IntDomainStorage, store: oscar.cp.CPStore) extends CPIntVar
{
  val realCPVar = {
    notInstantied match {
      case a: AdaptableIntDomainStorage => a
      case default => notInstantied
    }
  } match {
    case a: IntervalDomainStorage => oscar.cp.CPIntVar(a.min, a.max)(store)
    case b: SetDomainStorage => oscar.cp.CPIntVar(b.content)(store)
    case c: SingletonDomainStorage => oscar.cp.CPIntVar(c.max)(store)
  }

  /**
    * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
    */
  override def isBound: Boolean = realCPVar.isBound

  /**
    * @return A random value in the domain of the variable (uniform distribution)
    */
  override def randomValue(implicit rand: Random): Int = realCPVar.randomValue(rand)

  /**
    * @return  the maximum value in the domain
    */
  override def max: Int = realCPVar.max

  /**
    * @param value
    * @return the largest value < val in the domain, None if there is not value < val in the domain
    */
  override def valueBefore(value: Int): Option[Int] = Some(realCPVar.valueBefore(value)) //TODO: fixme

  /**
    * Return a representative name for this var(-like), if one was given
    */
  override def getRepresentativeName: Option[String] = notInstantied.getRepresentativeName

  /**
    * @return  the minimum value in the domain
    */
  override def min: Int = realCPVar.min

  /**
    * @param value
    * @return the smallest value > val in the domain, None if there is not value > val in the domain
    */
  override def valueAfter(value: Int): Option[Int] = Some(realCPVar.valueAfter(value)) //TODO: fixme

  override def iterator: Iterator[Int] = realCPVar.iterator

  /**
    * Test if a value is in the domain
    * @param value: value to test
    * @return  true if the domain contains the value val, false otherwise
    */
  override def hasValue(value: Int): Boolean = realCPVar.hasValue(value)
}