/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/

package oscar.cbls.invariants.core.computation

import oscar.cbls.invariants.core.propagation.Checker

import scala.collection.mutable.{Map => MMap}
import scala.language.implicitConversions

/** this is something that has an integer value.
  * this value can be queried, and invariants can be posted on it,
  * and it can be used on the righ hand of <== operator
  */
sealed trait IntValue extends Value{
  def value: Int
  def domain:Domain
  def min = domain.min
  def max = domain.max

  def name:String
  override def valueString: String = "" + value
  def restrictDomain(d:Domain): Unit

}

object IntValue {
  implicit def int2IntValue(a: Int): IntValue = CBLSIntConst(a)

  implicit def intArray2IntValueArray(a: Array[Int]): Array[CBLSIntConst] = a.map(CBLSIntConst(_))

  implicit def toFunction(i: IntValue): () => Int = () => i.value

  implicit val ord: Ordering[IntValue] = new Ordering[IntValue] {
    def compare(o1: IntValue, o2: IntValue) = {
      (o1, o2) match {
        case (a: CBLSIntConst, b: CBLSIntConst) => a.value compare b.value
        case (a: ChangingIntValue, b: ChangingIntValue) => a.uniqueID - b.uniqueID
        case (_:CBLSIntConst, _:ChangingIntValue) => -1
        case (_:ChangingIntValue, _:CBLSIntConst) => 1
      }
    }
  }
}

trait IntNotificationTarget{
  def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int)
}

/**An IntVar is a variable managed by the [[oscar.cbls.invariants.core.computation.Store]] whose type is integer.
  *
  * @param initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the value of the variable
  */
abstract class ChangingIntValue(initialValue:Int, initialDomain:Domain)
  extends AbstractVariable with IntValue{

  assert(initialDomain.contains(initialValue),initialValue+ " is not in the domain of "+this.name+"("+initialDomain+"). This might indicate an integer overflow.")
  
  private var privatedomain:Domain = initialDomain
  private var Value: Int = initialValue
  private var OldValue = Value

  def domain:Domain = privatedomain

  def restrictDomain(d:Domain): Unit = {
    privatedomain = privatedomain.restrict(d)
    if(!privatedomain.contains(Value)){
        this := privatedomain.min
      }
  }

  /*
   * Do NOT use this method! It exists only for the sake of oscar-fzn-cbls (hence it should not be suppressed either until oscar-fzn-cbls is able to use a workaround).
   */
  def relaxDomain(d: Domain): Domain = {
    val olddom = privatedomain
    privatedomain = d
    olddom
  }

  override def toString = {
    if(model != null && model.propagateOnToString) s"$name:=$value" else s"$name:=$Value"
  }
  override def toStringNoPropagate = s"$name:=$Value"

  def setValue(v:Int){
    if (v != Value){
      assert(domain.contains(v),v+ " is not in the domain of "+this+"("+min+".."+max+"). This might indicate an integer overflow.")
      Value = v
      notifyChanged()
    }
  }

  override def value: Int = {
    if (model == null) return Value
    if (definingInvariant == null && !model.propagating) return Value
    model.propagate(this)
    OldValue
  }

  def newValue:Int = {
    assert(model.checkExecutingInvariantOK(definingInvariant),"variable [" + this
      + "] queried for latest val by non-controlling invariant")
    Value
  }

  override def performPropagation(){performIntPropagation()}

  final protected def performIntPropagation(){
    if(OldValue!=Value){
      val old=OldValue
      OldValue=Value

      val dynListElements = getDynamicallyListeningElements
      val headPhantom = dynListElements.headPhantom
      var currentElement = headPhantom.next
      while(currentElement != headPhantom){
        val e = currentElement.elem
        currentElement = currentElement.next
        val inv:IntNotificationTarget = e._1.asInstanceOf[IntNotificationTarget]
        assert({this.model.NotifiedInvariant=inv.asInstanceOf[Invariant]; true})
        inv.notifyIntChanged(this,e._2,old,Value)
        assert({this.model.NotifiedInvariant=null; true})
      }
      /*
      for (e:((PropagationElement,Any)) <- getDynamicallyListeningElements){
      val inv:Invariant = e._1.asInstanceOf[Invariant]
        assert({this.model.NotifiedInvariant=inv; true})
        inv.notifyIntChangedAny(this,e._2,old,Value)
        assert({this.model.NotifiedInvariant=null; true})
      }*/
    }
  }

  override def checkInternals(c:Checker){
    c.check(OldValue == Value)
  }

  protected def :=(v: Int) {
    setValue(v)
  }

  protected def :+=(v: Int) {
    setValue(v + newValue)
  }

  protected def :*=(v: Int) {
    setValue(v * newValue)
  }

  protected def :-=(v:Int) {
    setValue(newValue - v)
  }

  /** increments the variable by one
    */
  protected def ++ {
    setValue(1 + newValue)
  }

  def compare(that: ChangingIntValue): Int = {
    assert(this.uniqueID != -1, "cannot compare non-registered PropagationElements this: [" + this + "] that: [" + that + "]")
    assert(that.uniqueID != -1, "cannot compare non-registered PropagationElements this: [" + this + "] that: [" + that + "]")
    this.uniqueID - that.uniqueID
  }
}

object ChangingIntValue{
  implicit val ord:Ordering[ChangingIntValue] = new Ordering[ChangingIntValue]{
    def compare(o1: ChangingIntValue, o2: ChangingIntValue) = o1.compare(o2)
  }
}

/**An IntVar is a variable managed by the [[oscar.cbls.invariants.core.computation.Store]] whose type is integer.
  *
  * @param givenModel is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[oscar.cbls.invariants.core.computation.CBLSIntConst]]
  * @param initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the initial value of the variable
  * @param n is the name of the variable, used for pretty printing only. if not set, a default will be used, based on the variable number
  */
class CBLSIntVar(givenModel: Store, initialValue: Int, initialDomain:Domain, n: String = null)
  extends ChangingIntValue(initialValue,initialDomain) with Variable{
  
  require(givenModel != null)

  model = givenModel

  override def name: String = if (n == null) defaultName else n

  override def :=(v: Int) {
    setValue(v)
  }

  override def :+=(v: Int) {
    setValue(v + newValue)
  }

  override def :*=(v: Int) {
    setValue(v * newValue)
  }

  override def :-=(v:Int) {
    setValue(newValue - v)
  }

  /** increments the variable by one
    */
  override def ++ {
    setValue(1 + newValue)
  }

  /**this operator swaps the value of two IntVar*/
  def :=:(v:CBLSIntVar){
    val a:Int = v.value
    v:=this.value
    this := a
  }

  /**this operator swaps the value of two IntVar*/
  def swap(v: CBLSIntVar) {
    this :=: v
  }

  def <==(i: IntValue) {IdentityInt(this,i)}
}

object CBLSIntVar{

  def apply(model: Store, value:Int = 0, d:Domain = FullRange, name:String = null) =
    new CBLSIntVar(model, value, d, name)

  implicit val ord:Ordering[CBLSIntVar] = new Ordering[CBLSIntVar]{
    def compare(o1: CBLSIntVar, o2: CBLSIntVar) = o1.compare(o2)
  }
}

/**
 * An IntConst is an [[oscar.cbls.invariants.core.computation.CBLSIntVar]] that has a constant value.
 * It has no associated model, as there is no need to incorporate it into any propagation process.
 * notice that you should not attempt to create a CBLSIntConst directly; use the companion object for an efficient memoïzation
* @param value: the value of the constant
* @author renaud.delandtsheer@cetic.be
*/
class CBLSIntConst(override val value:Int)
  extends IntValue{
  override def toString:String = "" + value
  override def domain: SingleValueDomain = new SingleValueDomain(value)
  override def min: Int = value
  override def max: Int = value
  override def name = "" + value
  override def restrictDomain(d:Domain){
    require(d.contains(value))
  }
}

object CBLSIntConst{
  implicit def int2IntValue(a: Int): IntValue = apply(a)
  implicit def int2IntConst(a: Int): CBLSIntConst = apply(a)
  //def apply(a:Int) = new CBLSIntConst(a)
  val constMap = MMap.empty[Int,CBLSIntConst]
  def apply(a:Int) = {
    if(constMap.contains(a))constMap(a)
    else{
      val res = new CBLSIntConst(a)
      constMap(a) = res
      res
    }
  }
}

/** this is a special case of invariant that has a single output variable, that is an IntVar
  * @author renaud.delandtsheer@cetic.be
  */
abstract class IntInvariant(initialValue:Int = 0, initialDomain:Domain = FullRange)
  extends ChangingIntValue(initialValue, initialDomain)
  with Invariant{

  override def definingInvariant: Invariant = this
  override def isControlledVariable:Boolean = true
  override def isDecisionVariable:Boolean = false

  override def model = propagationStructure.asInstanceOf[Store]

  override def hasModel:Boolean = schedulingHandler != null

  private var customName:String = null
  /**use this if you want to give a particular name to this concept, to be used in toString*/
  def setName(n:String):IntInvariant = {
    customName = n
    this
  }

  override final def name: String = if(customName == null) this.getClass.getSimpleName else customName

  override final def performPropagation(){
    performInvariantPropagation()
    performIntPropagation()
  }

  override def getDotNode:String = throw new Error("not implemented")
}

object IdentityInt{
  def apply(toValue:CBLSIntVar, fromValue:IntValue){
    fromValue match{
      case c:CBLSIntConst => toValue := c.value
      case c:ChangingIntValue => new IdentityInt(toValue, c)
    }
  }
}

/** an invariant that is the identity function
  * @author renaud.delandtsheer@cetic.be
  */
class IdentityInt(toValue:CBLSIntVar, fromValue:IntValue) extends Invariant with IntNotificationTarget{
  registerStaticAndDynamicDependency(fromValue)
  toValue.setDefiningInvariant(this)
  finishInitialization()

  toValue := fromValue.value

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    toValue := NewVal
  }

  override def checkInternals(c:Checker){
    c.check(toValue.value == fromValue.value)
  }
}
