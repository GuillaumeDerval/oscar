package oscar.cbls.business.geometry.invariants

import org.locationtech.jts.geom.Geometry
import oscar.cbls.business.geometry
import oscar.cbls.{IntValue, Store, Value}
import oscar.cbls.business.geometry.{CBLSGeometryInvariant, GeometryNotificationTarget}
import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker


case class IsWithin(inner:AtomicValue[Geometry], outer:AtomicValue[Geometry])
  extends Invariant with Constraint with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(inner)
  this.registerStaticAndDynamicDependency(outer)
  registerConstrainedVariables(inner, outer)

  finishInitialization()

  override val violation = new CBLSIntVar(model,0,0 to Int.MaxValue)


  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    violation := inner.value.difference(outer.value).getArea.toInt
  }

  override def violation(v: Value):IntValue = { if (inner == v || inner == v) violation else 0 }

  override def checkInternals(c: Checker): Unit = {}
}

class Union(store:Store,a:AtomicValue[Geometry],b:AtomicValue[Geometry])
  extends CBLSGeometryInvariant(store:Store,
    initialValue=a.value union b.value)
    with GeometryNotificationTarget {

  this.registerStaticAndDynamicDependency(a)
  this.registerStaticAndDynamicDependency(b)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value union b.value
  }
}

case class Intersection(store:Store,a:AtomicValue[Geometry],b:AtomicValue[Geometry], preCheck:Boolean)
  extends CBLSGeometryInvariant(store:Store,
    initialValue= if (preCheck && !(a.value intersects b.value)) geometry.emptyPolygon else a.value intersection b.value)
    with GeometryNotificationTarget {

  this.registerStaticAndDynamicDependency(a)
  this.registerStaticAndDynamicDependency(b)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := (if (preCheck && !(a.value intersects b.value)) geometry.emptyPolygon else a.value intersection b.value)
  }
}

class ConvexHull(store:Store,a:AtomicValue[Geometry])
  extends CBLSGeometryInvariant(store:Store,
    initialValue=a.value.convexHull())
    with GeometryNotificationTarget {

  this.registerStaticAndDynamicDependency(a)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value.convexHull()
  }
}

case class Area(store:Store,a:AtomicValue[Geometry])
  extends IntInvariant(
    initialValue = a.value.getArea.toInt,
    initialDomain = 0 to Int.MaxValue)
    with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(a)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value.getArea.toInt
  }
}


case class Length(store:Store,a:AtomicValue[Geometry])
  extends IntInvariant(
    initialValue = a.value.getEnvelope.getLength.toInt,
    initialDomain = 0 to Int.MaxValue)
    with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(a)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value.getEnvelope.getLength.toInt
  }
}

class Centroid(store:Store,shape:AtomicValue[Geometry])
  extends CBLSGeometryInvariant(
    store, initialValue = shape.value.getCentroid)
    with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(shape)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := shape.value.getCentroid
  }
}

class DistanceBetweenCentroids(store:Store,pointA:AtomicValue[Geometry],pointB:AtomicValue[Geometry])
  extends IntInvariant(
    initialValue = pointA.value.getCentroid.distance(pointB.value.getCentroid).toInt,
    initialDomain = 0 to Int.MaxValue)
    with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(pointA)
  this.registerStaticAndDynamicDependency(pointB)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := pointA.value.getCentroid.distance(pointB.value.getCentroid).toInt
  }
}

class DistanceBetweenShapes(store:Store,pointA:AtomicValue[Geometry],pointB:AtomicValue[Geometry])
  extends IntInvariant(
    initialValue = pointA.value.distance(pointB.value).toInt,
    initialDomain = 0 to Int.MaxValue)
    with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(pointA)
  this.registerStaticAndDynamicDependency(pointB)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := pointA.value.distance(pointB.value).toInt
  }
}

