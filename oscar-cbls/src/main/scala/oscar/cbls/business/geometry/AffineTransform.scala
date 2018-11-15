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




import oscar.cbls.core.draft.computation.Store
import oscar.cbls.core.draft.computation.core._


class AffineTransform {

  def andThen(other:AffineTransform):AffineTransform = ???

  def transform(p:Polygon):Polygon = ???
}

object AffineTransform {

  def translation(x:Int,y:Int):AffineTransform = ???
  def rotation(x:Int,y:Int):AffineTransform = ???

}


class AffineTransformVar(store: Store,
                         initialValue: AffineTransform,
                         givenName: String = null)
  extends CBLSAtomicVar[AffineTransform](store: Store,
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

trait AffineTransformNotificationTarget
  extends AtomicNotificationTarget[AffineTransform]{
  def notifyAffineTransformChanged(v: ChangingAtomicValue[AffineTransform],
                                   id: Int,
                                   OldVal: AffineTransform,
                                   NewVal: AffineTransform): Unit

  override def notifyAtomicChanged(v: ChangingAtomicValue[AffineTransform], id: Int,
                                   OldVal: AffineTransform,
                                   NewVal: AffineTransform): Unit ={
    notifyAffineTransformChanged(v: ChangingAtomicValue[AffineTransform],
      id: Int, OldVal: AffineTransform, NewVal: AffineTransform): Unit
  }
}

class AffineTransformConst(store:Store, override val value:AffineTransform)
  extends CBLSAtomicConst[AffineTransform](store, value){
  override def createClone: CBLSAtomicVar[AffineTransform] = this
}

class AffineTransformInvariant(store:Store,
                               initialValue:AffineTransform)
  extends AtomicInvariant[AffineTransform](store:Store, initialValue){

  override def createClone:AffineTransformVar = {
    val clone = new AffineTransformVar(
      store,
      this.value,
      "clone of " + this.name)

    clone <== this
    clone
  }
}

class Compose(store:Store,a:ChangingAtomicValue[AffineTransform],b:ChangingAtomicValue[AffineTransform])
  extends AffineTransformInvariant(
    store:Store,
    initialValue=a.value andThen b.value)
    with AffineTransformNotificationTarget {

  override def notifyAffineTransformChanged(v: ChangingAtomicValue[AffineTransform], id: Int, OldVal: AffineTransform, NewVal: AffineTransform): Unit = {
    this.scheduleMyselfForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value andThen b.value
  }
}



class Apply(store:Store,a:ChangingAtomicValue[AffineTransform],b:ChangingAtomicValue[Polygon])
  extends PolygonInvariant(
    store:Store,
    initialValue=a.value.transform(b.value))
    with AffineTransformNotificationTarget
    with PolygonNotificationTarget {

  override def notifyAffineTransformChanged(v: ChangingAtomicValue[AffineTransform], id: Int, OldVal: AffineTransform, NewVal: AffineTransform): Unit = {
    this.scheduleMyselfForPropagation()
  }


  override def notifyPolygonChanged(v: ChangingAtomicValue[Polygon], id: Int, OldVal: Polygon, NewVal: Polygon): Unit = {
    this.scheduleMyselfForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value.transform(b.value)
  }
}
