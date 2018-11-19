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
import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.{Checker, PropagationElement}
import oscar.cbls.{IntValue, Store, Value}

class CBLSGeometryVar(store: Store,
                      initialValue: Geometry,
                      givenName: String = null)
  extends CBLSAtomicVar[Geometry](store: Store,
    initialValue,
    givenName: String ){

  def createClone:CBLSGeometryVar = {
    val clone = new CBLSGeometryVar(
      store,
      this.value,
      "clone of " + this.name)

    clone <== this
    clone
  }

  def <== (g: ChangingAtomicValue[Geometry]): Unit ={
    new IdentityGeometry(this, g)
  }

  override def performNotificationToListeningInv(inv: PropagationElement, id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    val target = inv.asInstanceOf[GeometryNotificationTarget]
    target.notifyGeometryChange(this,id,oldVal,newVal)
  }
}

/** an invariant that is the identity function
  * @author renaud.delandtsheer@cetic.be
  */
class IdentityGeometry(toValue:CBLSGeometryVar, fromValue:ChangingAtomicValue[Geometry])
  extends Invariant with GeometryNotificationTarget{
  registerStaticAndDynamicDependency(fromValue)
  toValue.setDefiningInvariant(this)
  finishInitialization()

  toValue := fromValue.value


  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    toValue := newVal
  }

  override def checkInternals(c:Checker){
    c.check(toValue.value == fromValue.value)
  }
}





trait GeometryNotificationTarget{
  def notifyGeometryChange(a:ChangingAtomicValue[Geometry],id:Int,oldVal:Geometry,newVal:Geometry)
}


class CBLSGeometryConst(store:Store, override val value:Geometry)
  extends CBLSAtomicConst[Geometry](value){
}

class CBLSGeometryInvariant(store:Store,
                            initialValue:Geometry)
  extends AtomicInvariant[Geometry](initialValue){

  def createClone:CBLSGeometryVar = {
    val clone = new CBLSGeometryVar(
      store,
      this.value,
      "clone of " + this.name)

    clone <== this
    clone
  }

  override def performNotificationToListeningInv(inv: PropagationElement, id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    val target = inv.asInstanceOf[GeometryNotificationTarget]
    target.notifyGeometryChange(this,id,oldVal,newVal)
  }
}

case class IsWithin(inner:ChangingAtomicValue[Geometry], outer:ChangingAtomicValue[Geometry])
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


case class NoOverlap(shapes:Array[ChangingAtomicValue[Geometry]])
  extends Invariant with Constraint with GeometryNotificationTarget{

  for(shapeId <- shapes.indices){
    val shape = shapes(shapeId)
    this.registerStaticAndDynamicDependency(shape,shapeId)
    registerConstrainedVariable(shape)
  }

  finishInitialization()

  override val violation = new CBLSIntVar(model,0,0 to Int.MaxValue)

  val shapeViolation = Array.tabulate(shapes.length)(shapeID => {
    new CBLSIntVar(model,0,0 to Int.MaxValue,"numberOfOverlappingShapesWith_" + shapes(shapeID).name)
  })


  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    for(i <- shapes.indices) {
      shapeViolation(i) := 0
    }
    violation := 0
    //this is AWFULLY SLOW!!!
    val shapeValues = shapes.map(_.value)
    for(i <- shapes.indices){
      val shapei = shapeValues(i)
      for(j <- 0 until i){
        val shapej = shapeValues(j)

        if(shapei overlaps shapej){
          val surfaceError = shapei.intersection(shapej).getArea.toInt
          shapeViolation(i) :+= surfaceError
          shapeViolation(j) :+= surfaceError
          violation :+= surfaceError
        }
      }
    }
  }

  override def violation(v: Value):IntValue = {
    val shapeID = shapes.indexOf(v) //ok, this is crap
    if(shapeID == -1) 0 else shapeViolation(shapeID)
  }

  override def checkInternals(c: Checker): Unit = {}
}


class Union(store:Store,a:ChangingAtomicValue[Geometry],b:ChangingAtomicValue[Geometry])
  extends CBLSGeometryInvariant(store:Store,
    initialValue=a.value union b.value)
    with GeometryNotificationTarget {

  this.registerStaticAndDynamicDependency(a)
  this.registerStaticAndDynamicDependency(b)

  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value union b.value
  }
}

class Area(store:Store,a:ChangingAtomicValue[Geometry])
  extends IntInvariant(
    initialValue = a.value.getArea.toInt,
    initialDomain = 0 to Int.MaxValue)
    with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(a)

  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value.getArea.toInt
  }
}

