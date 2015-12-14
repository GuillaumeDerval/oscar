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

import oscar.cbls.invariants.core.propagation.{Checker, PropagationElement}

import scala.collection.immutable.SortedSet
import scala.language.implicitConversions

sealed trait SetValue extends Value{
  def value: SortedSet[Int]
  def domain:Domain
  def min = domain.min
  def max = domain.max
  def name:String
  override final def valueString: String = "{" + value.mkString(",") + "}"
}

object SetValue{
  implicit def intSet2IntSetVar(a:SortedSet[Int]):SetValue = CBLSSetConst(a)
  implicit def toFunction(i:SetValue):()=>SortedSet[Int] = () => i.value
}

abstract class ChangingSetValue(initialValue:SortedSet[Int], initialDomain:Domain)
  extends AbstractVariable with SetValue{
  private var privatedomain:Domain = initialDomain
  private var Value: SortedSet[Int] = initialValue
  private var OldValue:SortedSet[Int] = Value

  def domain:Domain = privatedomain

  /**this must be protected because invariants might rework this after isntanciation
    * for CBLSVars, no problems*/
  protected def restrictDomain(d:Domain): Unit ={
    privatedomain = privatedomain.restrict(d)
  }

  private var ToPerform: List[(Int,Boolean)] = List.empty

  private def Perform(){
    def Update(l:List[(Int,Boolean)]){
      if (l.isEmpty) return
      Update(l.tail)
      val (v, inserted) = l.head
      if (inserted) OldValue += v
      else OldValue -= v
    }
    Update(ToPerform)
    ToPerform = List.empty
  }

  override def toString:String = name + ":={" + (if(model.propagateOnToString) value else Value).mkString(",") + "}"

  /** this method is a toString that does not trigger a propagation.
    * use this when debugguing your software.
    * you should specify to your IDE to render variable objects using this method isntead of the toString method
    * @return a string similar to the toString method
    */
  def toStringNoPropagate: String = name + ":={" + Value.foldLeft("")(
    (acc,intval) => if(acc.equalsIgnoreCase("")) ""+intval else acc+","+intval) + "}"

  /**The values that have bee impacted since last propagation was performed.
    * null if set was assigned
    */
  var TouchedValues:List[(Int,Boolean)] = List.empty

  def insertValue(v:Int){
    if (!Value.contains(v)){
      if (TouchedValues != null) TouchedValues = (v,true) :: TouchedValues
      Value +=v
      notifyChanged()
    }
  }

  def deleteValue(v:Int){
    if (Value.contains(v)){
      if (TouchedValues != null) TouchedValues = (v,false) :: TouchedValues
      Value -=v
      notifyChanged()
    }
  }

  /**We suppose that the new value is not the same as the actual value.
    * otherwise, there is a huge waste of time.
    * @param v the new value to set to the variable
    */
  def setValue(v:SortedSet[Int]){
    TouchedValues = null
    Value = v
    notifyChanged()
  }

  override def performPropagation(){performSetPropagation()}

  final protected def performSetPropagation(){
    if(getDynamicallyListeningElements.isEmpty){
      //no need to do it gradually
      OldValue=Value
    }else{
      if (TouchedValues == null){
        //need to calll every listening one, so gradual approach required
        OldValue.diff(Value).foreach(v => {
          OldValue -=v
          val dynListElements = getDynamicallyListeningElements
          val headPhantom = dynListElements.headPhantom
          var currentElement = headPhantom.next
          while(currentElement != headPhantom){
            val e = currentElement.elem
            currentElement = currentElement.next
            val inv:Invariant = e._1.asInstanceOf[Invariant]
            assert({this.model.NotifiedInvariant=inv; true})
            inv.notifyDeleteOnAny(this,e._2,v)
            assert({this.model.NotifiedInvariant=null; true})
          }
        })
        //puis on fait partir tous les insert
        Value.diff(OldValue).foreach(v => {
          OldValue += v
          val dynListElements = getDynamicallyListeningElements
          val headPhantom = dynListElements.headPhantom
          var currentElement = headPhantom.next
          while(currentElement != headPhantom){
            val e = currentElement.elem
            currentElement = currentElement.next
            val inv:Invariant = e._1.asInstanceOf[Invariant]
            assert({this.model.NotifiedInvariant=inv; true})
            inv.notifyInsertOnAny(this,e._2,v)
            assert({this.model.NotifiedInvariant=null; true})
          }
        })
        //puis, on fait une affectation en plus, pour garbage collecter l'ancienne structure de donnees.
        assert(OldValue.intersect(Value).size == Value.size, "mismatch: OLD" + OldValue + " New:" + Value)
        OldValue=Value
      }else{
        //only touched values must be looked for
        for ((v,inserted) <- TouchedValues.reverse){
          //We simply replay the history. If some backtrack was performed, it is suboptimal
          // eg: if something was propagated to this during a neighbourhood exploration not involving this var
          if (inserted){
            //inserted
            ToPerform = (v, inserted) :: ToPerform
            val dynListElements = getDynamicallyListeningElements
            val headPhantom = dynListElements.headPhantom
            var currentElement = headPhantom.next
            while(currentElement != headPhantom){
              val e = currentElement.elem
              currentElement = currentElement.next
              val inv:Invariant = e._1.asInstanceOf[Invariant]

              assert({this.model.NotifiedInvariant=inv;true})
              inv.notifyInsertOnAny(this,e._2,v)
              assert({this.model.NotifiedInvariant=null;true})
            }
          }else{
            //deleted
            ToPerform = (v, inserted) :: ToPerform
            val dynListElements = getDynamicallyListeningElements
            val headPhantom = dynListElements.headPhantom
            var currentElement = headPhantom.next
            while(currentElement != headPhantom){
              val e = currentElement.elem
              currentElement = currentElement.next
              val inv:Invariant = e._1.asInstanceOf[Invariant]
              assert({this.model.NotifiedInvariant=inv;true})
              inv.notifyDeleteOnAny(this,e._2,v)
              assert({this.model.NotifiedInvariant=null;true})
            }
          }
        }
        OldValue = Value //in case we were lazy on the update
        ToPerform = List.empty
      }
    }
    TouchedValues = List.empty
  }

  def value:SortedSet[Int] = getValue(false)

  def getValue(NewValue:Boolean=false):SortedSet[Int] = {
    if (NewValue){
      assert(model.checkExecutingInvariantOK(definingInvariant),
        "variable [" + this + "] queried for latest val by non-controlling invariant")
      Value
    }else{
      if (model == null) return Value
      if (definingInvariant == null && !model.propagating) return Value
      model.propagate(this)
      Perform()
      OldValue
    }
  }

  /**We suppose that the new value is not the same as the actual value.
    * otherwise, there is a huge waste of time.
    * @param v the new value to set to the variable
    */
  protected def :=(v:SortedSet[Int]) {setValue(v)}

  protected def :+=(i:Int) {this.insertValue(i)}
  protected def :-=(i:Int) {this.deleteValue(i)}

  def getDotNode = "[label = \"IntSetVar(" + name + ")\" shape = oval color = " + getDotColor + "]"

  override def checkInternals(c:Checker){
    assert(this.definingInvariant == null || OldValue.intersect(Value).size == Value.size,
      "internal error: " + "Value: " + Value + " OldValue: " + OldValue)
  }
}

object ChangingSetValue{
  implicit val ord:Ordering[ChangingSetValue] = new Ordering[ChangingSetValue]{
    def compare(o1: ChangingSetValue, o2: ChangingSetValue) = o1.compare(o2)
  }
}

/**An IntSetVar is a variable managed by the [[oscar.cbls.invariants.core.computation.Store]] whose type is set of integer.
  * @param givenModel is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[oscar.cbls.invariants.core.computation.CBLSSetConst]]
  * @param initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the initial value of the variable
  * @param n is the name of the variable, used for pretty printing only. if not set, a default will be used, based on the variable number
  * */
class CBLSSetVar(givenModel: Store, initialValue: SortedSet[Int], initialDomain:Domain, n: String = null)
  extends ChangingSetValue(initialValue, initialDomain) with Variable{
  
  require(givenModel != null)
  
  model = givenModel

  override def restrictDomain(d:Domain) = super.restrictDomain(d)

  override def name: String = if (n == null) defaultName else n

  override def :=(v:SortedSet[Int]) {setValue(v)}

  override def :+=(i:Int) {this.insertValue(i)}
  override def :-=(i:Int) {this.deleteValue(i)}

  def <==(i: SetValue) {IdentitySet(this,i)}
}

object CBLSSetVar{
  //this conversion is forbidden because we inserted the new grammar.
  //implicit def toIntSet(v:IntSetVar):SortedSet[Int] = v.value

  def apply(d:Domain=FullRange, v:Iterable[Int] = List.empty, name:String="")(implicit s:Store) = {
    val emptySet:SortedSet[Int] = SortedSet.empty
    new CBLSSetVar(s, emptySet ++ v, d, name)
  }

  implicit val ord:Ordering[CBLSSetVar] = new Ordering[CBLSSetVar]{
    def compare(o1: CBLSSetVar, o2: CBLSSetVar) = o1.compare(o2)
  }
}

/**
 * An IntSetConst is an IntSetVar that has a constant value, defined by a set of integer.
 * It has no associated model, as there is no need to incorporate it into any propagation process.
 * @param value: the value of the constant
 * @author renaud.delandtsheer@cetic.be
 * */
case class CBLSSetConst(override val value:SortedSet[Int])
  extends SetValue{
  override def toString:String = "Set{" + value.mkString(",") + "}"
  override def domain:Domain = DomainRange(value.min,value.max)
  override val min: Int = if (value.isEmpty) Int.MaxValue else value.min
  override val max: Int = if(value.isEmpty) Int.MinValue else value.max
  override def name: String = toString
}


/*
* @author renaud.delandtsheer@cetic.be
 */
abstract class SetInvariant(initialValue:SortedSet[Int] = SortedSet.empty,
                            initialDomain:Domain = FullRange)
  extends ChangingSetValue(initialValue, initialDomain) with Invariant{

  override def definingInvariant: Invariant = this
  override def isControlledVariable:Boolean = true
  override def isDecisionVariable:Boolean = false

  override def model = propagationStructure.asInstanceOf[Store]
  override def hasModel:Boolean = schedulingHandler != null

  private var customName:String = null
  /**use this if you want to give a particular name to this concept, to be used in toString*/
  def setName(n:String):SetInvariant = {
    customName = n
    this
  }

  override final def name: String = if(customName == null) this.getClass.getSimpleName else customName

  override final def performPropagation(){
    performInvariantPropagation()
    performSetPropagation()
  }

  override def getDotNode:String = throw new Error("not implemented")
}

object IdentitySet{
  def apply(toValue:CBLSSetVar, fromValue:SetValue){
    fromValue match{
      case c:CBLSSetConst => toValue := c.value
      case c:ChangingSetValue => new IdentitySet(toValue, c)
    }
  }
}

/** an invariant that is the identity function
  * @author renaud.delandtsheer@cetic.be
  */
class IdentitySet(toValue:CBLSSetVar, fromValue:ChangingSetValue) extends Invariant{
  registerStaticAndDynamicDependency(fromValue)
  toValue.setDefiningInvariant(this)
  finishInitialization()

  toValue := fromValue.value

  override def notifyInsertOn(v:ChangingSetValue,value:Int){
    assert(v == this.fromValue)
    toValue.insertValue(value)
  }

  override def notifyDeleteOn(v:ChangingSetValue,value:Int){
    assert(v == this.fromValue)
    toValue.deleteValue(value)
  }

  override def checkInternals(c:Checker){
    c.check(toValue.value equals fromValue.value)
  }
}
