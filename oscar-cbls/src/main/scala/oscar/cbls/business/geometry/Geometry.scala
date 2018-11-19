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

import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory}
import org.locationtech.jts.geom.util.AffineTransformation
import oscar.cbls.core.draft.computation.{IntInvariant, Store}
import oscar.cbls.core.draft.computation.core._


object Geometry{
  def createCircle(r:Double,nbEdges:Int = 16, ensureCorrectSurface:Boolean = true):Geometry = {
    require(nbEdges >=4,"a circle is hard to approximate with less than four edges...")

    val pointRotation = AffineTransformation.rotationInstance((2*math.Pi)/nbEdges)

    val startPoint = new Coordinate(r, 0)
    var currentPoint:Coordinate = startPoint
    var allPoints:Array[Coordinate] = Array.fill(nbEdges+1)(null)
    allPoints(0) = currentPoint
    allPoints(nbEdges) = currentPoint

    for(currentIndice <- 1 until nbEdges){
      currentPoint = pointRotation.transform(currentPoint,new Coordinate(0, 0))
      allPoints(currentIndice) = currentPoint
    }

    val c1 = factory.createPolygon(allPoints)

    if(ensureCorrectSurface){
      val s1 = c1.getArea
      val factor = math.sqrt(math.Pi*r*r/s1)
      val scaling = AffineTransformation.scaleInstance(factor,factor)
      scaling.transform(c1)
    }else{
      c1
    }
  }
}

class GeometryVar(store: Store,
                initialValue: Geometry,
                givenName: String = null)
  extends CBLSAtomicVar[Geometry](store: Store,
    initialValue,
    givenName: String ){

  override def createClone:GeometryVar = {
    val clone = new GeometryVar(
      store,
      this.value,
      "clone of " + this.name)

    clone <== this
    clone
  }
}

class GeometryConst(store:Store, override val value:Geometry)
  extends CBLSAtomicConst[Geometry](store, value){
  override def createClone: CBLSAtomicVar[Geometry] = this
}

class GeometryInvariant(store:Store,
                      initialValue:Geometry)
  extends AtomicInvariant[Geometry](store:Store, initialValue){

  override def createClone:GeometryVar = {
    val clone = new GeometryVar(
      store,
      this.value,
      "clone of " + this.name)

    clone <== this
    clone
  }
}

class Union(store:Store,a:ChangingAtomicValue[Geometry],b:ChangingAtomicValue[Geometry]) extends
  GeometryInvariant(store:Store,
    initialValue=a.value union b.value)
  with AtomicNotificationTarget[Geometry] {

  a.registerStaticAndPermanentDynamicDependency(this)
  b.registerStaticAndPermanentDynamicDependency(this)

  override def notifyAtomicChanged(v: ChangingAtomicValue[Geometry], id: Int, OldVal: Geometry, NewVal: Geometry): Unit = {
    this.scheduleMyselfForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value union b.value
  }
}

class Area(store:Store,a:ChangingAtomicValue[Geometry])
  extends IntInvariant(store:Store,
    0,
    initialValue = a.value.getArea.toInt)
    with AtomicNotificationTarget[Geometry]{

  a.registerStaticAndPermanentDynamicDependency(this)


  override def notifyAtomicChanged(v: ChangingAtomicValue[Geometry], id: Int, OldVal: Geometry, NewVal: Geometry): Unit = {
    this.scheduleMyselfForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value.getArea.toInt
  }
}

