package oscar.modeling.vars.nostorage

import oscar.algo.vars.FloatVarLike
import oscar.modeling.vars.FloatVarImplem
import oscar.modeling.vars.domainstorage.DomainStorage

class NoFloatDomainStorage extends DomainStorage with FloatVarImplem with FloatVarLike
{
  def fail = throw new RuntimeException("This is a fake storage, and should'nt be instantiated")
  override def context = fail
  override def isBound: Boolean = fail
  override def hasValue(value: Double): Boolean = fail
  override def value(): Double = fail
  override def min: Double = fail
  override def max: Double = fail
  override def name: String = fail
}

