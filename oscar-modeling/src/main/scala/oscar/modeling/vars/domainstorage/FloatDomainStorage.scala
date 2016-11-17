package oscar.modeling.vars.domainstorage

import oscar.algo.vars.FloatVarLike
import oscar.modeling.misc.VariableNotBoundException
import oscar.modeling.vars.FloatVarImplem

class FloatDomainStorage(val min: Double, val max: Double, val name: String) extends DomainStorage with FloatVarImplem with FloatVarLike
{
  override def context = throw new Exception("This variable is not instantiated and thus has no context")

  /**
    * @return true if the domain of the variable has exactly one value,
    *         false if the domain has more than one value
    */
  override def isBound: Boolean = min == max

  /**
    * Test if a value is in the domain
    *
    * @param value : value to test
    * @return true if the domain contains the value val, false otherwise
    */
  override def hasValue(value: Double): Boolean = value >= min && value <= max

  /**
    * @return returns the set this variable represents, if it is bound
    */
  override def value(): Double = if(isBound) max else throw new VariableNotBoundException()
}

