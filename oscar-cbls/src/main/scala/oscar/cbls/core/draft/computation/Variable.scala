package oscar.cbls.core.draft.computation

import oscar.cbls.core.draft.propagation.PropagationElement

/**This is the base class for variable. A variable is a propagation element that holds some value.
  * Variables have an associated model, to which they register as soon as they are created. Variables also have a name,
  * which is used solely for printing models.
  */
trait AbstractVariable extends PropagationElement{

  final def model_=(s:Store): Unit ={
    s.registerVariable(this)
  }
  def model = propagationStructure.asInstanceOf[Store]

  def hasModel:Boolean = hasPropagationStructure

  def snapshot:AbstractVariableSnapShot

  def name:String

  def defaultName = s"Var_$uniqueID"

  protected def definingInvariant:Invariant

  def isControlledVariable:Boolean = definingInvariant != null
  def isDecisionVariable:Boolean = definingInvariant == null

  /** this method is a toString that does not trigger a propagation.
    * use this to debug your software.
    * you should specify to your IDE to render variable objects using this method isntead of the toString method
    * @return a string similar to the toString method
    */
  def toStringNoPropagate:String
}

object AbstractVariable{
  implicit val ord:Ordering[AbstractVariable] = new Ordering[AbstractVariable]{
    def compare(o1: AbstractVariable, o2: AbstractVariable) = o1.compare(o2) //that the compare of propagation element, actually
  }
}



//TODO: try to remove the double inclusion of AbstractVariable into CBLSIntVar and SetVar
trait Variable extends AbstractVariable{
  protected var definingInvariant:Invariant = null
  def setDefiningInvariant(i:Invariant){
    assert(i.model == model || i.model == null)
    assert(! i.isInstanceOf[Variable])
    require(definingInvariant == null)
    definingInvariant = i
    registerStaticallyListenedElement(i)
    registerDynamicallyListenedElement(i,0)
    }
}

abstract class AbstractVariableSnapShot(val variable:AbstractVariable){

  // Added by GO for pretty printing of some benchmarks:
  // to change if this affects the printing of other benchmarks
  override def toString: String = s"Variable[name:${variable.name}, value:${variable.valueString}]"

  final def restore() {
    variable match {
      case v : Variable if v.isDecisionVariable => doRestore()
      case _ => throw new Error("can only re-assign decision variable, not " + variable)
    }
  }

  final def restoreIfDecisionVariable(){
    variable match {
      case v : Variable if v.isDecisionVariable => doRestore()
      case _ => ;
    }
  }
  protected def doRestore()
}


object Variable{
  implicit val ord:Ordering[Variable] = new Ordering[Variable]{
    def compare(o1: Variable, o2: Variable) = o1.compare(o2) //that the compare of propagation element, actually
  }
}
