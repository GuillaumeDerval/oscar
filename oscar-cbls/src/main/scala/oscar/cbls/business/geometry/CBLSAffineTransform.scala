package oscar.cbls.business.geometry

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

import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.util.AffineTransformation
import oscar.cbls.Store
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.{Checker, PropagationElement}

/**
  *
  * @param store
  * @param initialValue is the initial value of the variable
  * @param givenName
  */
class CBLSAffineTransformVar(store: Store,
                             initialValue: AffineTransformation,
                             givenName: String = null)
  extends CBLSAtomicVar[AffineTransformation](store: Store,
    initialValue,
    givenName: String ){

  def createClone:CBLSAffineTransformVar = {
    val clone = new CBLSAffineTransformVar(
      store,
      this.value,
      "clone of " + this.name)

    clone <== this
    clone
  }

  def <== (g: ChangingAtomicValue[AffineTransformation]): Unit ={
    new IdentityAffineTransformation(this, g)
  }

  override def performNotificationToListeningInv(inv: PropagationElement, id: Int, oldVal: AffineTransformation, newVal: AffineTransformation): Unit = {
    val target = inv.asInstanceOf[AffineTransformNotificationTarget]
    target.notifyAffineTransformChange(this,id,oldVal,newVal)
  }
}

/** an invariant that is the identity function
  * @author renaud.delandtsheer@cetic.be
  */
class IdentityAffineTransformation(toValue:CBLSAffineTransformVar, fromValue:ChangingAtomicValue[AffineTransformation])
  extends Invariant with AffineTransformNotificationTarget{
  registerStaticAndDynamicDependency(fromValue)
  toValue.setDefiningInvariant(this)
  finishInitialization()

  toValue := fromValue.value


  override def notifyAffineTransformChange(a: ChangingAtomicValue[AffineTransformation], id: Int, oldVal: AffineTransformation, newVal: AffineTransformation): Unit = {
    toValue := newVal
  }

  override def checkInternals(c:Checker){
    c.check(toValue.value == fromValue.value)
  }
}

trait AffineTransformNotificationTarget{
  def notifyAffineTransformChange(a:ChangingAtomicValue[AffineTransformation],id:Int,oldVal:AffineTransformation,newVal:AffineTransformation)
}


class CBLSAffineTransformConst(store:Store, override val value:AffineTransformation)
  extends CBLSAtomicConst[AffineTransformation](value){
}

class CBLSAffineTransformInvariant(initialValue:AffineTransformation)
  extends AtomicInvariant[AffineTransformation](initialValue){

  def createClone:CBLSAffineTransformVar = {
    val clone = new CBLSAffineTransformVar(
      this.model,
      this.value,
      "clone of " + this.name)

    clone <== this
    clone
  }

  override def performNotificationToListeningInv(inv: PropagationElement, id: Int, oldVal: AffineTransformation, newVal: AffineTransformation): Unit = {
    val target = inv.asInstanceOf[AffineTransformNotificationTarget]
    target.notifyAffineTransformChange(this,id,oldVal,newVal)
  }
}

class Compose(store:Store,a:ChangingAtomicValue[AffineTransformation],b:ChangingAtomicValue[AffineTransformation])
  extends CBLSAffineTransformInvariant(
    initialValue = new AffineTransformation(a.value) compose b.value)
    with AffineTransformNotificationTarget {

  this.registerStaticAndDynamicDependency(a)
  this.registerStaticAndDynamicDependency(b)

  finishInitialization(store)

  override def notifyAffineTransformChange(a: ChangingAtomicValue[AffineTransformation], id: Int, oldVal: AffineTransformation, newVal: AffineTransformation): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := new AffineTransformation(a.value) compose b.value
  }
}

class Translation(store:Store,x:IntValue,y:IntValue)
  extends CBLSAffineTransformInvariant(
    initialValue = AffineTransformation.translationInstance(x.value.toDouble,y.value.toDouble))
    with IntNotificationTarget {

  this.registerStaticAndDynamicDependency(x)
  this.registerStaticAndDynamicDependency(y)
  finishInitialization(store)

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit = {
    println("notified translation component changed")
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    println("updated translation")
    this := AffineTransformation.translationInstance(x.value.toDouble,y.value.toDouble)
  }
}

class Apply(store:Store,a:AtomicValue[AffineTransformation],b:AtomicValue[Geometry])
  extends CBLSGeometryInvariant(
    store:Store,
    initialValue=a.value.transform(b.value))
    with GeometryNotificationTarget
    with AffineTransformNotificationTarget{

  this.registerStaticAndDynamicDependency(a)
  this.registerStaticAndDynamicDependency(b)

  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def notifyAffineTransformChange(a: ChangingAtomicValue[AffineTransformation], id: Int, oldVal: AffineTransformation, newVal: AffineTransformation): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value.transform(b.value)
  }
}
