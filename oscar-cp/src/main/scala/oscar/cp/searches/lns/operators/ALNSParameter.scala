package oscar.cp.searches.lns.operators

/**
  * This class defines an Adaptive large neighbourhood search parameter.
  */
class ALNSParameter[T](val value: T, failThreshold: Int) extends ALNSElement(failThreshold){

  override def setActive(state: Boolean): Unit = {
    if(!state) println("Parameter " + value + " deactivated")
    super.setActive(state)
  }

  //Two parameters are considered equals if their value is equal
  override def equals(obj: scala.Any): Boolean = obj match{
    case parameter: ALNSParameter[T] => value.equals(parameter.value)
    case _ => false
  }
}
