package oscar.cbls.business.geometry.invariants

import org.locationtech.jts.geom.Geometry
import oscar.cbls.{IntValue, Store, Value}
import oscar.cbls.business.geometry.old.OverlapDetection.{OverlapError, checkOverlapAndInsertNewShapeIntoList}
import oscar.cbls.business.geometry.old.Shape
import oscar.cbls.core.computation.{AtomicValue, CBLSIntVar, ChangingAtomicValue, Invariant}
import oscar.cbls.core.constraint.{Constraint, ConstraintSystem}
import oscar.cbls.core.propagation.Checker

object NoOverlap{

  def apply(store:Store,shapes:Array[AtomicValue[Geometry]]):ConstraintSystem = {

    val c = new ConstraintSystem(store)

    for(i <- shapes.indices){
      for(j <- 0 until i){
        c.add(NoOverlapN2(Array(shapes(i),shapes(j))))
      }
    }
    for(shape <- shapes) {
      c.violation(shape)
    }

    c.close()
    c
  }

}

case class NoOverlapN2(shapes:Array[AtomicValue[Geometry]])
  extends Invariant with Constraint with GeometryNotificationTarget{

  for(shapeId <- shapes.indices){
    val shape = shapes(shapeId)
    this.registerStaticAndDynamicDependency(shape,shapeId)
    registerConstrainedVariable(shape)
  }

  finishInitialization()

  override val violation = new CBLSIntVar(model,0,0 to Int.MaxValue)
  violation.setDefiningInvariant(this)

  val shapeViolation = Array.tabulate(shapes.length)(shapeID => {
    val tmp = new CBLSIntVar(model,0,0 to Int.MaxValue,"overlap(" + shapes(shapeID).name + ")")
    tmp.setDefiningInvariant(this)
    tmp
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

        if(shapei intersects shapej){
          val surfaceError = shapei.intersection(shapej).getArea.toInt
          shapeViolation(i) :+= surfaceError
          shapeViolation(j) :+= surfaceError
          violation :+= surfaceError
        }
      }
    }
    //println("end overlap algo")
  }

  //initial propagation
  performInvariantPropagation()

  override def violation(v: Value):IntValue = {
    val shapeID = shapes.indexOf(v) //ok, this is crap
    if(shapeID == -1) 0 else shapeViolation(shapeID)
  }

  override def checkInternals(c: Checker): Unit = {}

  override def toString: String = {
    "NoOverlap(\n\t" + shapes.indices.map(i => shapeViolation(i)).mkString("\n\t") + "\n)"
  }
}


case class NoOverlapSweep(shapes:Array[AtomicValue[Geometry]])
  extends Invariant with Constraint with GeometryNotificationTarget{

  for(shapeId <- shapes.indices){
    val shape = shapes(shapeId)
    this.registerStaticAndDynamicDependency(shape,shapeId)
    registerConstrainedVariable(shape)
  }

  finishInitialization()

  override val violation = new CBLSIntVar(model,0,0 to Int.MaxValue)
  violation.setDefiningInvariant(this)

  val shapeViolation = Array.tabulate(shapes.length)(shapeID => {
    val tmp = new CBLSIntVar(model,0,0 to Int.MaxValue,"overlap(" + shapes(shapeID).name + ")")
    tmp.setDefiningInvariant(this)
    tmp
  })

  override def notifyGeometryChange(a: ChangingAtomicValue[Geometry], id: Int, oldVal: Geometry, newVal: Geometry): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    def isOverlapByLineSweep(shapes:List[Shape]): Boolean = {

      var shapesSortedByMinX = shapes.sortBy(_.minX)
      var openShapesSortedByClosingX: List[Shape] = List.empty

      while (shapesSortedByMinX.nonEmpty) {
        val nextOpeningShape = shapesSortedByMinX.head
        val nextClosingShapeOpt: Option[Shape] = openShapesSortedByClosingX.headOption

        if (nextClosingShapeOpt.isEmpty || nextOpeningShape.minX <= nextClosingShapeOpt.get.maxX) {
          //open a new shape, and insert at the right lace in the list (insertion sort)
          try {
            openShapesSortedByClosingX = checkOverlapAndInsertNewShapeIntoList(openShapesSortedByClosingX,nextOpeningShape)
          } catch {
            case o: OverlapError =>
              println(o)
              return true
          }
          shapesSortedByMinX = shapesSortedByMinX.tail
        } else {
          //close shape
          openShapesSortedByClosingX = openShapesSortedByClosingX.tail
        }
      }
      false // no overlap detected
    }
  }

  //initial propagation
  performInvariantPropagation()

  override def violation(v: Value):IntValue = {
    val shapeID = shapes.indexOf(v) //ok, this is crap
    if(shapeID == -1) 0 else shapeViolation(shapeID)
  }

  override def checkInternals(c: Checker): Unit = {}

  override def toString: String = {
    "NoOverlap(\n\t" + shapes.indices.map(i => shapeViolation(i)).mkString("\n\t") + "\n)"
  }
}
