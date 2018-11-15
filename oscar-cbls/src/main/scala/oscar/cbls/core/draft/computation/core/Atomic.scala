package oscar.cbls.core.draft.computation.core

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

import oscar.cbls.core.draft.computation._
import oscar.cbls.core.draft.propagation.{KeyForDynamicDependencyRemoval, PropagationElement, VaryingDependencies}

import scala.language.implicitConversions

trait AtomicNotificationTarget[T]{
  def notifyAtomicChanged(v: ChangingAtomicValue[T], id: Int, OldVal: T, NewVal: T)
}

/**An IntVar is a variable managed by the [[oscar.cbls.core.computation.Store]] whose type is integer.
  *
  * @param initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the value of the variable
  */
abstract class ChangingAtomicValue[@specialized(Int) T](val store:Store,
                                                           initialValue:T)
  extends ChangingValue(store) {

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  //snapshot
  override def snapshot : ChangingAtomicValueSnapshot[T] =
    new ChangingAtomicValueSnapshot[T](this,this.value)

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // toString

  override def valueString(blockPropagation: Boolean): String =
    "" + (if(blockPropagation) mOldValue else value)

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // value management
  private[this] var mNewValue: T = initialValue
  private[this] var mOldValue = mNewValue

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // getting value

  def value: T = {
    ensureUpToDate()
    mOldValue
  }

  protected def newValue:T = mNewValue

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // setting value

  @inline
  protected def value_=(v:T):Unit = {
    if (v != mNewValue){
      mNewValue = v
      scheduleMyselfForPropagation()
    }
  }

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  //registration

  def registerStaticAndPermanentDynamicDependency(target:PropagationElement with AtomicNotificationTarget[T],
                                                  id:Int = 0): Unit ={
    super.registerStaticallyListeningElement(target)
    super.registerPermanentDynamicDependency(target,id)
  }

  def registerStaticDependency(pe:PropagationElement with VaryingDependencies): Unit ={
    super.registerStaticallyListeningElement(pe)
  }

  def registerTemporaryDynamicDependency(target:AtomicNotificationTarget[T] with VaryingDependencies,
                                         id:Int=0): KeyForDynamicDependencyRemoval = {
    super.registerTemporaryDynamicDependency(target,id)
  }

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // propagation
  override def performPropagation(){performAtomicPropagation()}

  final protected def performAtomicPropagation(){
    if(mOldValue!=mNewValue){
      val old=mOldValue
      mOldValue=mNewValue

      val dynListElements = this.dynamicallyListeningElements
      val headPhantom = dynListElements.headPhantom
      var currentElement = headPhantom.next
      while(currentElement != headPhantom){
        val e = currentElement.elem
        val inv:AtomicNotificationTarget[T] = e._1.asInstanceOf[AtomicNotificationTarget[T]]
        inv.notifyAtomicChanged(this,e._2,old,mNewValue)
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
  protected def :=(v: T) {
    this.value = v
  }

  def createClone:CBLSAtomicVar[T]
}

class ChangingAtomicValueSnapshot[@specialized(Int) T](val variable:ChangingAtomicValue[T],
                                                          val savedValue:T)
  extends ChangingValueSnapshot(variable){

  override protected def doRestore() : Unit =
    throw new Error("cannot reload changing int values, only CBLSAtomicVar")
}

class AtomicVarSnapshot[@specialized(Int) T](variable:CBLSAtomicVar[T],
                                                savedValue:T)
  extends ChangingAtomicValueSnapshot[T](variable,savedValue){

  override protected def doRestore() : Unit = {
    variable := savedValue
  }
}


/**An AtomicVar is a variable managed by the [[oscar.cbls.core.computation.Store]] whose type is integer.
  *
  * @param givenModel is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[CBLSAtomicConst[T]]
  * @param initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the initial value of the variable
  * @param n is the name of the variable, used for pretty printing only. if not set, a default will be used, based on the variable number
  * @tparam T
  */
abstract class CBLSAtomicVar[@specialized(Int) T](store: Store,
                                            initialValue: T,
                                            givenName: String = null)
  extends ChangingAtomicValue[T](store, initialValue) with Variable{

  require(store != null)

  override def snapshot : AtomicVarSnapshot[T] =
    new AtomicVarSnapshot[T](this,this.value)

  override def name: String = if (givenName == null) super.name else givenName


  override def value_=(v: T): Unit = super.value_=(v)

  override def :=(v: T) {
    this.value = v
  }

  /**this operator swaps the value of two AtomicVar*/
  def :=:(v:CBLSAtomicVar[T]){
    val a:T = v.value
    v:=this.value
    this := a
  }

  /**this operator swaps the value of two AtomicVar*/
  def swap(v: CBLSAtomicVar[T]) {
    this :=: v
  }

  def <==(i: ChangingAtomicValue[T]) {IdentityAtomic[T](this,i,store)}
}

/**
  * An AtomicConst is an [[CBLSAtomicVar]] that has a constant value.
  * It has no associated model, as there is no need to incorporate it into any propagation process.
  * notice that you should not attempt to create a CBLSAtomicConst directly; use the companion object for an efficient memoïzation
  * @param value: the value of the constant
  * @author renaud.delandtsheer@cetic.be
  */
abstract class CBLSAtomicConst[@specialized(Int) T](store:Store, override val value:T)
  extends CBLSAtomicVar[T](store, value, "constant_" + value) {
  override protected def value_=(v: T): Unit =
    throw new Error("you cannot change the value of a constant")
}

/** this is a special case of invariant that has a single output variable, that is an AtomicVar
  * @author renaud.delandtsheer@cetic.be
  */
abstract class AtomicInvariant[@specialized(Int) T](store:Store,
                                                       initialValue:T = 0)
  extends ChangingAtomicValue[T](store,initialValue)
    with InvariantTrait{

  override final def performPropagation(){
    performInvariantPropagation()
    performAtomicPropagation()
  }
}

object IdentityAtomic{
  def apply[T](toValue:CBLSAtomicVar[T], fromValue:ChangingAtomicValue[T], store:Store){
    fromValue match{
      case c:CBLSAtomicConst[T] => toValue := c.value
      case c:ChangingAtomicValue[T] => new IdentityAtomic[T](toValue, c,store)
    }
  }
}

/** an invariant that is the identity function
  * @author renaud.delandtsheer@cetic.be
  */
class IdentityAtomic[@specialized(Int) T](toValue:CBLSAtomicVar[T], fromValue:ChangingAtomicValue[T], store:Store)
  extends Invariant(store)
    with AtomicNotificationTarget[T]{

  fromValue.registerStaticAndPermanentDynamicDependency(this)
  defineOutputVariable(toValue)

  toValue := fromValue.value

  override def notifyAtomicChanged(v: ChangingAtomicValue[T], id:Int, OldVal: T, NewVal: T) {
    toValue := NewVal
  }

  override def checkInternals(){
    require(toValue.value == fromValue.value)
  }
}
