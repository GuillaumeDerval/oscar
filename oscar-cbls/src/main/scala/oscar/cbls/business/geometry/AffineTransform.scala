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
  extends CBLSAtomicConst[AffineTransformation](value){
}

class AffineTransformInvariant(initialValue:AffineTransformation)
  extends AtomicInvariant[AffineTransformation](initialValue){

  override def createClone:AffineTransformVar = {
    val clone = new AffineTransformVar(
      this.model,
      this.value,
      "clone of " + this.name)

    clone <== this
    clone
  }
}

class Compose(store:Store,a:ChangingAtomicValue[AffineTransformation],b:ChangingAtomicValue[AffineTransformation])
  extends AffineTransformInvariant(
    initialValue = new AffineTransformation(a.value) compose b.value)
    with AtomicNotificationTarget[AffineTransformation] {

  this.registerStaticAndDynamicDependency(a)
  this.registerStaticAndDynamicDependency(b)

  override def notifyAtomicChanged(v: ChangingAtomicValue[AffineTransformation], id: Int, OldVal: AffineTransformation, NewVal: AffineTransformation): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := new AffineTransformation(a.value) compose b.value
  }
}

/*
class Apply(store:Store,a:ChangingAtomicValue[AffineTransformation],b:ChangingAtomicValue[Geometry])
  extends GeometryInvariant(
    store:Store,
    initialValue=a.value.transform(b.value))
    with AtomicNotificationTarget[AffineTransformation]
    with  AtomicNotificationTarget[Geometry] {

  this.registerStaticAndDynamicDependency(a)
  this.registerStaticAndDynamicDependency(b)

  override def notifyAtomicChanged(v: ChangingAtomicValue[AffineTransformation], id: Int, OldVal: AffineTransformation, NewVal: AffineTransformation): Unit = {
    this.scheduleForPropagation()
  }

  override def notifyAtomicChanged(v: ChangingAtomicValue[Geometry], id: Int, OldVal: Geometry, NewVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value.transform(b.value)
  }
}
*/