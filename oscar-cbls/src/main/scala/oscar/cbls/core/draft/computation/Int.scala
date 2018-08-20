package oscar.cbls.core.draft.computation

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



import oscar.cbls._
import oscar.cbls.core.computation.FullRange
import oscar.cbls.core.draft.propagation.{KeyForDynamicDependencyRemoval, PropagationElement, VaryingDependencies}

import scala.language.implicitConversions


trait IntNotificationTarget{
  def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int)
}

/**An IntVar is a variable managed by the [[oscar.cbls.core.computation.Store]] whose type is integer.
  *
  * @param initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the value of the variable
  */
abstract class ChangingIntValue(store:Store,
                                initialValue:Int,
                                initialDomain:Domain)
  extends ChangingValue(store) {

  require(initialDomain.contains(initialValue),
    initialValue+ " is not in the domain of "+this.name+"("+initialDomain+"). This might indicate an integer overflow.")

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // domain management
  private[this] var privateDomain:Domain = initialDomain


  def domain:Domain = privateDomain
  def min = domain.min
  def max = domain.max

  protected def restrictDomain(d:Domain): Unit = {
    privateDomain = privateDomain.intersect(d)
    require(privateDomain.contains(mNewValue),"you are restricting a domain and the new value is not in this domain")
  }

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  //snapshot
  override def snapshot : ChangingIntValueSnapshot =
    new ChangingIntValueSnapshot(this,this.value)

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // toString

  override def valueString(blockPropagation: Boolean): String =
    "" + (if(blockPropagation) mOldValue else value)

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // value management
  private[this] var mNewValue: Int = initialValue
  private[this] var mOldValue = mNewValue

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // getting value

  def value: Int = {
    ensureUpToDate()
    mOldValue
  }

  protected def newValue:Int = mNewValue

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // setting value

  @inline
  protected def setValue(v:Int){
    if (v != mNewValue){
      require(domain.contains(v),v+ " is not in the domain of "+this+"("+min+".."+max+"). This might indicate an integer overflow.")
      mNewValue = v
      scheduleMyselfForPropagation()
    }
  }

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  //registration

  def registerStaticAndPermanentDynamicDependency(target:PropagationElement with IntNotificationTarget,
                                                  id:Int = 0): Unit ={
    super.registerStaticallyListeningElement(target)
    super.registerPermanentDynamicDependency(target,id)
  }

  def registerStaticDependency(pe:PropagationElement with VaryingDependencies): Unit ={
    super.registerStaticallyListeningElement(pe)
  }

  def registerTemporaryDynamicDependency(target:IntNotificationTarget with VaryingDependencies,
                                         id:Int=0): KeyForDynamicDependencyRemoval = {
    super.registerTemporaryDynamicDependency(target,id)
  }

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // propagation


  override def performPropagation(){performIntPropagation()}

  final protected def performIntPropagation(){
    if(mOldValue!=mNewValue){
      val old=mOldValue
      mOldValue=mNewValue

      val dynListElements = this.dynamicallyListeningElements
      val headPhantom = dynListElements.headPhantom
      var currentElement = headPhantom.next
      while(currentElement != headPhantom){
        val e = currentElement.elem
        val inv:IntNotificationTarget = e._1.asInstanceOf[IntNotificationTarget]
        inv.notifyIntChanged(this,e._2,old,mNewValue)
        //we go to the next to be robust against invariant that change their dependencies when notified
        //this might cause crash because dynamicallyListenedInvariants is a mutable data structure
        currentElement = currentElement.next
      }
    }
  }

  override def checkInternals(){
    require(mOldValue == mNewValue)
  }


  // ////////////////////////////////////////////////////////////////////////////////////////////////
  //clean methods
  protected def :=(v: Int) {
    setValue(v)
  }

  protected def :+=(v: Int) {
    setValue(v + mNewValue)
  }

  protected def :*=(v: Int) {
    setValue(v * mNewValue)
  }

  protected def :-=(v:Int) {
    setValue(mNewValue - v)
  }

  /** increments the variable by one
    */
  protected def ++ {
    setValue(1 + mNewValue)
  }

  def createClone:CBLSIntVar = {
    val clone = new CBLSIntVar(
      store,
      this.value,
      this.domain,
      "clone of " + this.name)

    clone <== this
    clone
  }
}

object ChangingIntValue{
  implicit val ord:Ordering[ChangingIntValue] = new Ordering[ChangingIntValue]{
    def compare(o1: ChangingIntValue, o2: ChangingIntValue) = o1.compare(o2)
  }
}

class ChangingIntValueSnapshot(val variable:ChangingIntValue,
                               val savedValue:Int)
  extends ChangingValueSnapshot(variable){

  override protected def doRestore() : Unit =
    throw new Error("cannot reload changing int values, only CBLSIntVar")
}

class IntVarSnapshot(variable:CBLSIntVar,
                     savedValue:Int)
  extends ChangingIntValueSnapshot(variable,savedValue){

  override protected def doRestore() : Unit = {
    variable := savedValue
  }
}


/**An IntVar is a variable managed by the [[oscar.cbls.core.computation.Store]] whose type is integer.
  *
  * @param givenModel is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[oscar.cbls.core.computation.CBLSIntConst]]
  * @param initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the initial value of the variable
  * @param n is the name of the variable, used for pretty printing only. if not set, a default will be used, based on the variable number
  */
class CBLSIntVar(store: Store,
                 initialValue: Int,
                 initialDomain:Domain,
                 givenName: String = null)
  extends ChangingIntValue(store, initialValue, initialDomain) with Variable{
  
  require(store != null)

  override def snapshot : IntVarSnapshot =
    new IntVarSnapshot(this,this.value)

  override def name: String = if (givenName == null) super.name else givenName

  override def restrictDomain(d: Domain): Unit = super.restrictDomain(d)

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

  def <==(i: ChangingIntValue) {IdentityInt(this,i,store)}
}

object CBLSIntVar{
  implicit val ord:Ordering[CBLSIntVar] = new Ordering[CBLSIntVar]{
    def compare(o1: CBLSIntVar, o2: CBLSIntVar) = o1.compare(o2)
  }
}

/**
 * An IntConst is an [[oscar.cbls.core.computation.CBLSIntVar]] that has a constant value.
 * It has no associated model, as there is no need to incorporate it into any propagation process.
 * notice that you should not attempt to create a CBLSIntConst directly; use the companion object for an efficient memoïzation
* @param value: the value of the constant
* @author renaud.delandtsheer@cetic.be
*/
class CBLSIntConst(store:Store, override val value:Int)
  extends CBLSIntVar(store, value, value to value, "constant_" + value) {
  override protected def setValue(v: Int): Unit =
    throw new Error("you cannot change the value of a constant")
}

/** this is a special case of invariant that has a single output variable, that is an IntVar
  * @author renaud.delandtsheer@cetic.be
  */
abstract class IntInvariant(store:Store,
                            initialValue:Int = 0,
                            initialDomain:Domain = FullRange)
  extends ChangingIntValue(store,initialValue, initialDomain)
  with InvariantTrait{

  override final def performPropagation(){
    performInvariantPropagation()
    performIntPropagation()
  }
}

object IdentityInt{
  def apply(toValue:CBLSIntVar, fromValue:ChangingIntValue, store:Store){
    fromValue match{
      case c:CBLSIntConst => toValue := c.value
      case c:ChangingIntValue => new IdentityInt(toValue, c,store)
    }
  }
}

/** an invariant that is the identity function
  * @author renaud.delandtsheer@cetic.be
  */
class IdentityInt(toValue:CBLSIntVar, fromValue:ChangingIntValue, store:Store)
  extends Invariant(store)
    with IntNotificationTarget{

  fromValue.registerStaticAndPermanentDynamicDependency(this)
  defineOutputVariable(toValue)

  toValue := fromValue.value

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    toValue := NewVal
  }

  override def checkInternals(){
    require(toValue.value == fromValue.value)
  }
}
