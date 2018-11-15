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


class Polygon {

  def union(p:Polygon):Polygon = ???
  def inter(p:Polygon):Polygon = ???
  def overlap(p:Polygon):Boolean = ???

}

object Polygon{
  def circle(nbEdges:Int,radius:Int,keepSurfaceRatio:Boolean):Polygon = ???
}

class PolygonVar(store: Store,
                initialValue: Polygon,
                givenName: String = null)
  extends CBLSAtomicVar[Polygon](store: Store,
    initialValue,
    givenName: String ){

  override def createClone:PolygonVar = {
    val clone = new PolygonVar(
      store,
      this.value,
      "clone of " + this.name)

    clone <== this
    clone
  }
}

trait PolygonNotificationTarget
  extends AtomicNotificationTarget[Polygon]{

  def notifyPolygonChanged(v: ChangingAtomicValue[Polygon], id: Int, OldVal: Polygon, NewVal: Polygon): Unit

  override def notifyAtomicChanged(v: ChangingAtomicValue[Polygon], id: Int, OldVal: Polygon, NewVal: Polygon): Unit ={
    notifyPolygonChanged(v: ChangingAtomicValue[Polygon], id: Int, OldVal: Polygon, NewVal: Polygon): Unit
  }
}

class PolygonConst(store:Store, override val value:Polygon)
  extends CBLSAtomicConst[Polygon](store, value){
  override def createClone: CBLSAtomicVar[Polygon] = this
}



class PolygonInvariant(store:Store,
                      initialValue:Polygon)
  extends AtomicInvariant[Polygon](store:Store, initialValue){

  override def createClone:PolygonVar = {
    val clone = new PolygonVar(
      store,
      this.value,
      "clone of " + this.name)

    clone <== this
    clone
  }
}




class Union(store:Store,a:ChangingAtomicValue[Polygon],b:ChangingAtomicValue[Polygon]) extends
  PolygonInvariant(store:Store,
    initialValue=a.value union b.value) with PolygonNotificationTarget {

  override def notifyPolygonChanged(v: ChangingAtomicValue[Polygon], id: Int, OldVal: Polygon, NewVal: Polygon): Unit = {
    this.scheduleMyselfForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value union b.value
  }
}

object a {
  val s:Store = ???
  val x: ChangingAtomicValue[Polygon] = ???
  val y: ChangingAtomicValue[Polygon] = ???


  val t: ChangingAtomicValue[Polygon] = new Union(s,x,y)


}


