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


class CBLSAffineTransformConst(store:Store, override val value:AffineTransformation, givenName:String = null)
  extends CBLSAtomicConst[AffineTransformation](value){

  override def name = if (givenName == null) value.toString else givenName
  override def toString:String = if (givenName == null) value.toString else givenName
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
