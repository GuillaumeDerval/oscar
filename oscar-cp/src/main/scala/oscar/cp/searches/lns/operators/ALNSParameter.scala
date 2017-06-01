package oscar.cp.searches.lns.operators

/**
  * This class defines an Adaptive large neighbourhood search parameter.
  *
  * @param value The value of the parameter (should be unique for parameters used in the same adaptive store)
  */
class ALNSParameter[T](val value: T, failThreshold: Int) extends ALNSElement(failThreshold){

  //Two parameters are considered equals if their value is equal
  override def equals(obj: scala.Any): Boolean = obj match{
    case parameter: ALNSParameter[T] => value.equals(parameter.value)
    case _ => false
  }

  override def hashCode(): Int = value.hashCode()
}
