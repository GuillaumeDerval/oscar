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
import oscar.cbls.core.draft.computation.Store
import oscar.cbls.core.draft.computation.core._


object AffineTransformationObj{
  def translation(x:Int,y:Int):AffineTransformation = AffineTransformation.translationInstance(x,y)
  def rotation(tensOfDegrees:Int):AffineTransformation = AffineTransformation.rotationInstance((2*math.Pi)/(tensOfDegrees.toDouble / 3600))
}

/**
  *
  * @param store
  * @param initialValue is the initial value of the variable
  * @param givenName
  */
class AffineTransformVar(store: Store,
                         initialValue: AffineTransformation,
                         givenName: String = null)
  extends CBLSAtomicVar[AffineTransformation](store: Store,
    initialValue,
    givenName: String ){

  override def createClone:AffineTransformVar = {
    val clone = new AffineTransformVar(
      store,
      this.value,
      "clone of " + this.name)

    clone <== this
    clone
  }
}

class AffineTransformConst(store:Store, override val value:AffineTransformation)
  extends CBLSAtomicConst[AffineTransformation](store, value){
  override def createClone: CBLSAtomicVar[AffineTransformation] = this
}

class AffineTransformInvariant(store:Store,
                               initialValue:AffineTransformation)
  extends AtomicInvariant[AffineTransformation](store:Store, initialValue){

  override def createClone:AffineTransformVar = {
    val clone = new AffineTransformVar(
      store,
      this.value,
      "clone of " + this.name)

    clone <== this
    clone
  }
}

class Compose(store:Store,a:ChangingAtomicValue[AffineTransformation],b:ChangingAtomicValue[AffineTransformation])
  extends AffineTransformInvariant(
    store:Store,
    initialValue= new AffineTransformation(a.value) compose b.value)
    with AtomicNotificationTarget[AffineTransformation] {

  a.registerStaticAndPermanentDynamicDependency(this)
  b.registerStaticAndPermanentDynamicDependency(this)


  override def notifyAtomicChanged(v: ChangingAtomicValue[AffineTransformation], id: Int, OldVal: AffineTransformation, NewVal: AffineTransformation): Unit = {
    this.scheduleMyselfForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := new AffineTransformation(a.value) compose b.value
  }
}

class Apply(store:Store,a:ChangingAtomicValue[AffineTransformation],b:ChangingAtomicValue[Geometry])
  extends GeometryInvariant(
    store:Store,
    initialValue=a.value.transform(b.value))
    with GeometryNotificationTarget
    with AffineTransformNotificationTarget {

  a.registerStaticAndPermanentDynamicDependency(this)  //TODO: check if we really cannot have multiple atomic notification target on the came class!!! (alternatively, atomic notifiation targer should accept anything and perform some manual cast)
  b.registerStaticAndPermanentDynamicDependency(this)

  override def notifyAffineTransformChanged(v: ChangingAtomicValue[AffineTransformation], id: Int, OldVal: AffineTransformation, NewVal: AffineTransformation): Unit = {
    this.scheduleMyselfForPropagation()
  }

  override def notifyGeometryChanged(v: ChangingAtomicValue[Geometry], id: Int, OldVal: Geometry, NewVal: Geometry): Unit = {
    this.scheduleMyselfForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value.transform(b.value)
  }
}
