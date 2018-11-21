package oscar.cbls.business.geometry.invariants

import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.util.AffineTransformation
import oscar.cbls.Store
import oscar.cbls.business.geometry.{AffineTransformNotificationTarget, CBLSAffineTransformInvariant, CBLSGeometryInvariant, GeometryNotificationTarget}
import oscar.cbls.core.computation._


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
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
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
