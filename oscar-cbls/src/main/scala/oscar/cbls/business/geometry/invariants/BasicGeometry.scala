package oscar.cbls.business.geometry.invariants

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
import oscar.cbls.business.geometry
import oscar.cbls.{IntValue, Store, Value}
import oscar.cbls.business.geometry.model.{CBLSGeometryInvariant, GeometryNotificationTarget, GeometryValue}
import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker


case class IsWithin(inner:AtomicValue[Geometry], outer:AtomicValue[GeometryValue])
  extends Invariant with Constraint with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(inner)
  this.registerStaticAndDynamicDependency(outer)
  registerConstrainedVariables(inner, outer)

  finishInitialization()

  override val violation = new CBLSIntVar(model,0,0 to Int.MaxValue)


  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    violation := inner.value.difference(outer.value.geometry).getArea.toInt
  }

  override def violation(v: Value):IntValue = { if (inner == v || inner == v) violation else 0 }

  override def checkInternals(c: Checker): Unit = {}
}

class Union(store:Store,a:AtomicValue[GeometryValue],b:AtomicValue[GeometryValue])
  extends CBLSGeometryInvariant(store:Store,
    initialValue=new GeometryValue(a.value.geometry union b.value.geometry,false)())
    with GeometryNotificationTarget {

  this.registerStaticAndDynamicDependency(a)
  this.registerStaticAndDynamicDependency(b)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := new GeometryValue(a.value.geometry union b.value.geometry)()
  }
}

case class Intersection(store:Store,a:AtomicValue[GeometryValue],b:AtomicValue[GeometryValue], preCheck:Boolean)
  extends CBLSGeometryInvariant(store:Store,
    initialValue =
      if (preCheck && ((a.value.centroid distance b.value.centroid) > (a.value.overApproximatingRadius + b.value.overApproximatingRadius)))
        geometry.emptyGeometryValue
      else new GeometryValue(a.value.geometry intersection b.value.geometry)())
    with GeometryNotificationTarget {

  this.registerStaticAndDynamicDependency(a)
  this.registerStaticAndDynamicDependency(b)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this :=  (if (preCheck && ((a.value.centroid distance b.value.centroid) > (a.value.overApproximatingRadius + b.value.overApproximatingRadius)))
      geometry.emptyGeometryValue
    else new GeometryValue(a.value.geometry intersection b.value.geometry)())
  }
}

class ConvexHull(store:Store,a:AtomicValue[GeometryValue])
  extends CBLSGeometryInvariant(store:Store,
    initialValue = new GeometryValue(a.value.geometry.convexHull())())
    with GeometryNotificationTarget {

  this.registerStaticAndDynamicDependency(a)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := new GeometryValue(a.value.geometry.convexHull())()
  }
}

case class Area(store:Store,a:AtomicValue[GeometryValue])
  extends IntInvariant(
    initialValue = a.value.geometry.getArea().toInt,
    initialDomain = 0 to Int.MaxValue)
    with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(a)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this :=  a.value.geometry.getArea().toInt
  }
}

case class Length(store:Store,a:AtomicValue[GeometryValue])
  extends IntInvariant(
    initialValue = a.value.getEnvelope.getLength.toInt,
    initialDomain = 0 to Int.MaxValue)
    with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(a)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value.getEnvelope.getLength.toInt
  }
}

class Centroid(store:Store,shape:AtomicValue[GeometryValue])
  extends CBLSGeometryInvariant(
    store,
    initialValue = {
      val c = shape.value.centroid
      new GeometryValue(c)(inputCentroid = Some(c), inputOverApproximatingRadius= Some(0.0))
    }
  ) with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(shape)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    val c = shape.value.centroid
    this := new GeometryValue(c)(inputCentroid = Some(c), inputOverApproximatingRadius= Some(0.0))
  }
}

class DistanceBetweenCentroids(store:Store,pointA:AtomicValue[GeometryValue],pointB:AtomicValue[GeometryValue])
  extends IntInvariant(
    initialValue = pointA.value.centroid.distance(pointB.value.centroid).toInt,
    initialDomain = 0 to Int.MaxValue)
    with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(pointA)
  this.registerStaticAndDynamicDependency(pointB)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := pointA.value.centroid.distance(pointB.value.centroid).toInt
  }
}
