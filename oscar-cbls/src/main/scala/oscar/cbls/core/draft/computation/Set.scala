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


package oscar.cbls.core.draft.computation

import oscar.cbls.algo.dll._
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.core.computation.{Domain, FullRange}
import oscar.cbls.core.draft.propagation.{KeyForDynamicDependencyRemoval, PropagationElement, VaryingDependencies}
import oscar.cbls.core.propagation._

import scala.collection.immutable.SortedSet
import scala.language.implicitConversions



trait SetNotificationTarget extends PropagationElement{
  /**
    * this method will be called just before the variable "v" is actually updated.
    * @param v
    * @param d d is always MinValue when notified for a valueWiseKey
    * @param addedValues
    * @param removedValues
    * @param oldValue
    * @param newValue
    */
  def notifySetChanges(v: ChangingSetValue,
                       id: Int,
                       addedValues: Iterable[Int],
                       removedValues: Iterable[Int],
                       oldValue: SortedSet[Int],
                       newValue: SortedSet[Int])
}


/*
  def registerDynamicValueWiseDependency(s:SetValue):ValueWiseKey = {
    s match{
      case c:ChangingSetValue =>
        val key = registerDynamicallyListenedElement(c,Int.MinValue)
        val valueWiseKey = c.instrumentKeyToValueWiseKey(key)
        valueWiseKey.target = this
        valueWiseKey
      case _ =>
        DoNothingValueWiseKey
    }
  }
 */

abstract class ChangingSetValue(store:Store,
                                initialValue:SortedSet[Int],
                                initialDomain:Domain)
  extends ChangingValue(store){

  require(initialValue.forall(v => initialDomain.contains(v)),
    initialValue+ " is not fully included in the domain of " + this.name + "("+initialDomain+"). This might indicate an integer overflow.")

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // domain management

  private var privateDomain:Domain = initialDomain

  //Since this value is often used, we keep it permanently here
  private[this] var domainSizeDiv10 = privateDomain.size/10

  def domain:Domain = privateDomain
  def min = domain.min
  def max = domain.max

  protected def restrictDomain(d:Domain): Unit ={
    privateDomain = privateDomain.intersect(d)
    domainSizeDiv10 = privateDomain.size/10
  }

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  //snapshot

  override def snapshot : ChangingSetValueSnapShot =
    new ChangingSetValueSnapShot(this,this.value)

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // toString

  override final def valueString(blockPropagation:Boolean):String =
    "{" + (if(blockPropagation) mOldValue else value).mkString(",") + "}"

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // value management

  private var mNewValue: SortedSet[Int] = initialValue
  private var mOldValue:SortedSet[Int] = mNewValue

  //more data to capture the delta incrementally
  private[this] var addedValues:QList[Int] = null
  private[this] var removedValues:QList[Int] = null
  private[this] var nbTouched:Int = 0


  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // getting value

  def value:SortedSet[Int] = {
    ensureUpToDate()
    mOldValue
  }

  protected def newValue:SortedSet[Int] = mNewValue

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // setting value

  /**We suppose that the new value is not the same as the actual value.
    * otherwise, there is a huge waste of time.
    * @param v the new value to set to the variable
    */
  protected def setValue(v:SortedSet[Int]){
    removedValues = null
    addedValues = null
    nbTouched = -1
    mNewValue = v
    scheduleMyselfForPropagation()
  }

  protected def insertValue(v:Int){
    if (!mNewValue.contains(v)) {
      insertValueNotPreviouslyIn(v)
    }
  }

  protected def insertValueNotPreviouslyIn(v:Int){
    if (nbTouched != -1){
      addedValues = QList(v,addedValues)
      nbTouched += 1
      if(nbTouched > domainSizeDiv10) nbTouched = -1
    }
    mNewValue += v
    scheduleMyselfForPropagation()
  }


  protected def deleteValue(v:Int){
    if (mNewValue.contains(v)){
      deleteValuePreviouslyIn(v)
    }
  }

  protected def deleteValuePreviouslyIn(v:Int){
    if (nbTouched != -1){
      removedValues = QList(v,removedValues)
      nbTouched += 1
      if(nbTouched > domainSizeDiv10) nbTouched = -1
    }
    mNewValue -=v
    scheduleMyselfForPropagation()
  }

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  //registration

  def registerStaticAndPermanentDynamicDependency(target:PropagationElement with SetNotificationTarget,
                                                  id:Int = 0): Unit ={
    require(id != Int.MinValue, "MinValue is forbidden as an id for Set values")
    super.registerStaticallyListeningElement(target)
    super.registerPermanentDynamicDependency(target,id)
  }

  def registerStaticDependency(pe:PropagationElement with VaryingDependencies): Unit ={
    super.registerStaticallyListeningElement(pe)
  }

  def registerTemporaryDynamicDependency(target:SetNotificationTarget with VaryingDependencies,
                                         id:Int=0): KeyForDynamicDependencyRemoval = {
    require(id != Int.MinValue, "MinValue is forbidden as an id for Set values")
    super.registerTemporaryDynamicDependency(target,id)
  }

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  //value-wise registration




  def registerStaticAndPermanentDynamicDependencyValueWise(target:PropagationElement with SetNotificationTarget,
                                                           id:Int = 0): ValueWiseKey ={
    require(id != Int.MinValue, "MinValue is forbidden as an id for Set values")

    super.registerStaticallyListeningElement(target)
    super.registerPermanentDynamicDependency(target,Int.MinValue)

    createValueWiseMechanicsIfNeeded()
    new ValueWiseKey(this, target, id)
  }

  def registerTemporaryDynamicDependencyValueWise(target:SetNotificationTarget with VaryingDependencies,
                                                  id:Int=0): TemporaryValueWiseKey = {
    require(id != Int.MinValue, "MinValue is forbidden as an id for Set values")

    createValueWiseMechanicsIfNeeded()
    new TemporaryValueWiseKey(this,
      target,
      id,
      super.registerTemporaryDynamicDependency(target,Int.MinValue))
  }

  //mechanism that manage non value-wise keys (since we do not want to iterate on them)
  private var listeningElementsNonValueWise:DoublyLinkedList[(PropagationElement,Int)] = null

  private def usesValueWise:Boolean = valueToValueWiseKeys != null

  private def createValueWiseMechanicsIfNeeded(){
    if(!usesValueWise){
      valueToValueWiseKeys
        = Array.tabulate(this.domain.max - this.domain.min + 1)(_ => new DoublyLinkedList[ValueWiseKey]())

      listeningElementsNonValueWise
        = dynamicallyListeningElements.permaFilter({case (pe,id) => id != Int.MinValue})
    }
  }
  private[this] var valueToValueWiseKeys:Array[DoublyLinkedList[ValueWiseKey]] = null
  private[this] val offsetForValueWiseKey = domain.min

  @inline
  protected[ValueWiseKey] def addToValueWiseKey(key:ValueWiseKey,
                                                value:Int):DLLStorageElement[ValueWiseKey] = {
    valueToValueWiseKeys(value - offsetForValueWiseKey).addElem(key)
  }

  @inline
  private def valueWiseKeysAtValue(value:Int):DoublyLinkedList[ValueWiseKey] = valueToValueWiseKeys(value - offsetForValueWiseKey)

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  // propagation

  override def performPropagation(){performSetPropagation()}

  @inline
  final protected def performSetPropagation(){
    if(dynamicallyListeningElements.isEmpty){
      //no need to do it gradually
      mOldValue = mNewValue
    }else{
      val (addedValues,deletedValues):(Iterable[Int],Iterable[Int]) = if (nbTouched == -1) {
        //need to call every listening one, so gradual approach required
        if(mNewValue == mOldValue) (List.empty,List.empty)
        else (mNewValue.diff(mOldValue),mOldValue.diff(mNewValue))

      }else {
        //TODO: this is slow, and it delives TreeSet. TreeSet are slow fo valueWise notifications
        //we have the set of values that have been touched (added or deleted)
        //but we need to check for each opf them if they have been both added and deleted
        var addedUnique = SortedSet.empty[Int] ++ this.addedValues
        var removedUnique = SortedSet.empty[Int] ++ this.removedValues
        for(inter <- addedUnique.intersect(removedUnique)){
          val inNew = mNewValue.contains(inter)
          val inOld = mOldValue.contains(inter)
          if(!inOld || inNew) {
            removedUnique = removedUnique - inter
          }
          if(!inNew || inOld) {
            addedUnique = addedUnique - inter
          }
        }
        (addedUnique,removedUnique)
      }

      assert((mOldValue ++ addedValues -- deletedValues).equals(mNewValue))

      if(addedValues.nonEmpty || deletedValues.nonEmpty) {

        //notifying the PE that listen to the whole set
        val dynListElementsForWholeSetNotification:Iterable[(PropagationElement,Int)] =
          if(usesValueWise) listeningElementsNonValueWise else dynamicallyListeningElements

        for((inv,id) <- dynListElementsForWholeSetNotification){
          //TODO: the added and deleted could be computed lazily (you never know...)
          //TODO: the data struct can be improved.
          inv.asInstanceOf[SetNotificationTarget].notifySetChanges(this, id, addedValues, deletedValues, mOldValue, mNewValue)
        }

        //value wise notifications
        if(usesValueWise) {
          val currentValueWisePropagationWaveIdentifier = new ValueWisePropagationWaveIdentifier()

          valueWiseNotify(addedValues, addedValues, deletedValues, mOldValue, mNewValue, currentValueWisePropagationWaveIdentifier)
          valueWiseNotify(deletedValues, addedValues, deletedValues, mOldValue, mNewValue, currentValueWisePropagationWaveIdentifier)
        }
      }
      //puis, on fait une affectation en plus, pour garbage collecter l'ancienne structure de donnees.
      mOldValue=mNewValue
    }
    this.addedValues = null
    this.removedValues = null
    nbTouched = 0
  }

  @inline  //This method is awfully slow, ad we do not know why
  private def valueWiseNotify(valuesForValueWise : Iterable[Int],
                              addedValues:Iterable[Int],
                              deletedValues:Iterable[Int],
                              oldValue: SortedSet[Int],
                              newValue: SortedSet[Int],
                              currentValueWisePropagationWaveIdentifier:ValueWisePropagationWaveIdentifier) {
    val valuesIt = valuesForValueWise.iterator
    while(valuesIt.hasNext){
      val value = valuesIt.next()
      val valueWiseKeys = valueWiseKeysAtValue(value)
      val headPhantom = valueWiseKeys.phantom
      var currentElement : DLLStorageElement[ValueWiseKey] = headPhantom.next
      while (currentElement != headPhantom) {
        val e : ValueWiseKey = currentElement.elem
        if(e.currentValueWisePropagationWaveIdentifier != currentValueWisePropagationWaveIdentifier) {
          e.currentValueWisePropagationWaveIdentifier = currentValueWisePropagationWaveIdentifier
          val target = e.target

          target.notifySetChanges(this, Int.MinValue, addedValues, deletedValues, oldValue, newValue)

        }
        //we go to the next to be robust against invariant that change their dependencies when notified
        //this might cause crash because dynamicallyListenedInvariants is a mutable data structure
        currentElement = currentElement.next
      }
    }
  }

  override def checkInternals(){
    require(this.staticallyListenedElements == null || mOldValue.intersect(mNewValue).size == mNewValue.size,
      "internal error: " + "Value: " + mNewValue + " OldValue: " + mOldValue)
  }

  // ////////////////////////////////////////////////////////////////////////////////////////////////
  //clean methods

  protected def :=(v:SortedSet[Int]) {setValue(v)}

  protected def :+=(i:Int) {this.insertValue(i)}

  protected def :-=(i:Int) {this.deleteValue(i)}

  def createClone:CBLSSetVar = {
    val clone = new CBLSSetVar(store, this.value, this.domain, "clone of " + this.name)
    clone <== this
    clone
  }
}

object ChangingSetValue{
  implicit def toFunction(i:ChangingSetValue):()=>SortedSet[Int] = () => i.value

  implicit val ord:Ordering[ChangingSetValue] = new Ordering[ChangingSetValue]{
    def compare(o1: ChangingSetValue, o2: ChangingSetValue) = o1.compare(o2)
  }
}

class ChangingSetValueSnapShot(val variable:ChangingSetValue,
                               val savedValue:SortedSet[Int])
  extends ChangingValueSnapshot(variable){

  override protected def doRestore() : Unit =
    throw new Error("cannot reload changing set values, only CBLSSetVar")
}

class SetVarSnapShot(variable:CBLSSetVar,
                     savedValue:SortedSet[Int])
  extends ChangingSetValueSnapShot(variable, savedValue){

  override protected def doRestore() : Unit = {
    variable := savedValue
  }
}

class ValueWisePropagationWaveIdentifier()

class TemporaryValueWiseKey(setValue:ChangingSetValue,
                            target:SetNotificationTarget,
                            id:Int,
                            originalKey:KeyForDynamicDependencyRemoval)
  extends ValueWiseKey(setValue, target, id){

  def performRemove(){
    //remove all values in the focus of this key
    //TODO: this is too slow, but probably not called very often, so no need to trade memory for this?
    for(i <- valueToKeyArray){
      if(i != null) i.delete()
    }
    originalKey.performRemove()
  }
}

class ValueWiseKey(setValue:ChangingSetValue,
                   val target:SetNotificationTarget,
                   id:Int){

  private val sizeOfSet = setValue.max - setValue.min +1
  private val offset = -setValue.min

  var currentValueWisePropagationWaveIdentifier:ValueWisePropagationWaveIdentifier = null

  protected val valueToKeyArray = Array.fill[DLLStorageElement[ValueWiseKey]](sizeOfSet)(null)

  def addToKey(value:Int) {
    //TODO: change to assert
    require(valueToKeyArray(value + offset) == null, "already listening to this value")
    valueToKeyArray(value + offset) = setValue.addToValueWiseKey(this,value)
  }

  def removeFromKey(value:Int){
    require(valueToKeyArray(value + offset) != null, "was not listening to this value")
    val k = valueToKeyArray(value + offset)
    k.delete()
    valueToKeyArray(value + offset) = null
  }
}


/**An IntSetVar is a variable managed by the [[oscar.cbls.core.computation.Store]] whose type is set of integer.
  * @param givenModel is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[oscar.cbls.core.computation.CBLSSetConst]]
  * @param initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the initial value of the variable
  * @param n is the name of the variable, used for pretty printing only. if not set, a default will be used, based on the variable number
  * */
class CBLSSetVar(store: Store,
                 initialValue: SortedSet[Int],
                 initialDomain:Domain,
                 givenName: String = null)
  extends ChangingSetValue(store, initialValue, initialDomain) with Variable{

  require(store != null)

  override def snapshot: SetVarSnapShot =
    new SetVarSnapShot(this,this.value)

  override def name: String = if (givenName == null) super.name else givenName

  override def restrictDomain(d:Domain) = super.restrictDomain(d)

  override def :=(v:SortedSet[Int]) {
    setValue(v)
  }

  override def :+=(i:Int) {
    this.insertValue(i)
  }

  override def :-=(i:Int) {
    this.deleteValue(i)
  }

  override def insertValue(v : Int) {
    super.insertValue(v)
  }

  override def insertValueNotPreviouslyIn(v : Int) {
    super.insertValueNotPreviouslyIn(v)
  }

  override def deleteValue(v : Int) {
    super.deleteValue(v)
  }

  override def deleteValuePreviouslyIn(v : Int) {
    super.deleteValuePreviouslyIn(v)
  }

  /** We suppose that the new value is not the same as the actual value.
    * otherwise, there is a huge waste of time.
    * @param v the new value to set to the variable
    */
  override def setValue(v : SortedSet[Int]) : Unit = super.setValue(v)

  override def value : SortedSet[Int] = super.value

  def <==(i: ChangingSetValue) {IdentitySet(this,i,store)}

}

object CBLSSetVar{
  //this conversion is forbidden because we inserted the new grammar.
  //implicit def toIntSet(v:IntSetVar):SortedSet[Int] = v.value

  implicit val ord:Ordering[CBLSSetVar] = new Ordering[CBLSSetVar]{
    override def compare(x: CBLSSetVar, y: CBLSSetVar): Int = x.compare(y)
  }
}


/**
  * An IntSetConst is an IntSetVar that has a constant value, defined by a set of integer.
  * It has no associated model, as there is no need to incorporate it into any propagation process.
  * @param value: the value of the constant
  * @author renaud.delandtsheer@cetic.be
  * */
class CBLSSetConst(store:Store, override val value:SortedSet[Int])
  extends CBLSSetVar(store,value,value,"set_constant{"+value.mkString(",")+"}"){

  override def setValue(v: SortedSet[Int]): Unit =
    throw new Error("you cannot change the value of a constant")
}

/*
* @author renaud.delandtsheer@cetic.be
 */
abstract class SetInvariant(store:Store,
                            initialValue:SortedSet[Int] = SortedSet.empty,
                            initialDomain:Domain = FullRange)
  extends ChangingSetValue(store, initialValue, initialDomain)
    with InvariantTrait {

  override final def performPropagation(){
    performInvariantPropagation()
    performSetPropagation()
  }
}

object IdentitySet{
  def apply(toValue:CBLSSetVar, fromValue:ChangingSetValue, store:Store){
    fromValue match{
      case c:CBLSSetConst => toValue := c.value
      case c:ChangingSetValue => new IdentitySet(toValue, c, store)
    }
  }
}

/** an invariant that is the identity function
  * @author renaud.delandtsheer@cetic.be
  */
class IdentitySet(toValue:CBLSSetVar, fromValue:ChangingSetValue, store:Store)
  extends Invariant(store)
    with SetNotificationTarget{

  fromValue.registerStaticAndPermanentDynamicDependency(this)
  defineOutputVariable(toValue)

  toValue := fromValue.value

  override def notifySetChanges(v: ChangingSetValue,
                                d: Int,
                                addedValues: Iterable[Int],
                                removedValues: Iterable[Int],
                                oldValue: SortedSet[Int],
                                newValue: SortedSet[Int]) : Unit = {
    assert(v == this.fromValue)
    for(added <- addedValues)toValue.insertValueNotPreviouslyIn(added)
    for(deleted <- removedValues) toValue.deleteValuePreviouslyIn(deleted)
  }

  override def checkInternals(){
    require(toValue.value equals fromValue.value)
  }
}
