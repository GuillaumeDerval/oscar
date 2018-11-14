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
import oscar.cbls.core.draft.computation.core.{AtomicInvariant, AtomicNotificationTarget, CBLSAtomicConst, CBLSAtomicVar}


class AffineTransform {


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
  extends AtomicNotificationTarget[AffineTransform]

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

