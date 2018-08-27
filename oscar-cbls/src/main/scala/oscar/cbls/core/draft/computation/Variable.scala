package oscar.cbls.core.draft.computation

import oscar.cbls.core.draft.propagation.PropagationElement

/**This is the base class for variable. A variable is a propagation element that holds some value.
  * Variables have an associated model, to which they register as soon as they are created. Variables also have a name,
  * which is used solely for printing models.
  */
abstract class ChangingValue(store:Store) extends PropagationElement{

  store.registerChangingValue(this)

  def snapshot:ChangingValueSnapshot

  def name:String = s"ChangingValue$uniqueID" //this is a default name, in case no other more precise one is given

  def valueString(blockPropagation:Boolean = false):String

  override def toString: String = name + ":" + valueString()

  /** this method is a toString that does not trigger a propagation.
    * use this to debug your software.
    * you should specify to your IDE to render variable objects using this method instead of the toString method
    * @return a string similar to the toString method
    */
  final def toStringNoPropagate:String = name + ":" + valueString(true)
}

object ChangingValue{
  implicit val ord:Ordering[ChangingValue] = new Ordering[ChangingValue]{
    def compare(o1: ChangingValue, o2: ChangingValue) = o1.compare(o2)
  }
}

trait Variable extends ChangingValue{

  override def name:String = s"Variable$uniqueID" //this is a default name; it can of course be changed

  final override def toString: String = super.toString

  final def hasDefiningInvariant:Boolean = staticallyListenedElements != null
}

object Variable{
  implicit val ord:Ordering[Variable] = new Ordering[Variable]{
    def compare(o1: Variable, o2: Variable) = o1.compare(o2) //that the compare of propagation element, actually
  }
}

abstract class ChangingValueSnapshot(val changingValue:ChangingValue){

  final def restore() {
    changingValue match {
      case v : Variable if !v.hasDefiningInvariant => doRestore()
      case _ => throw new Error("can only re-assign decision variable, not " + changingValue)
    }
  }

  final def restoreIfDecisionVariable(){
    changingValue match {
      case v : Variable if !v.hasDefiningInvariant => doRestore()
      case _ => ;
    }
  }

  protected def doRestore()
}
