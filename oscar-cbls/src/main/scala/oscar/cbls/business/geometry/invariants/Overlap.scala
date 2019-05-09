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


import org.locationtech.jts.algorithm.distance.{DistanceToPoint, PointPairDistance}
import org.locationtech.jts.geom.prep.{PreparedGeometry, PreparedGeometryFactory}
import org.locationtech.jts.geom.{Geometry, GeometryCollection, Point, Polygon}
import oscar.cbls.{CBLSIntVar, IntValue}
import oscar.cbls.algo.magicArray.IterableMagicBoolArray
import oscar.cbls.business.geometry
import oscar.cbls.business.geometry.model._
import oscar.cbls.core.{IntInvariant, Invariant}
import oscar.cbls.core.computation.{AtomicValue, ChangingAtomicValue}

object Overlap {

  def holesBetween(s:List[Geometry]):List[Geometry] = {
    var acc = s.head
    var t = s.tail
    while(t.nonEmpty){
      acc = acc union t.head
      t = t.tail
    }
    extractHoles(acc)
  }

  private def extractHoles(g:Geometry):List[Geometry] = {
    g match {
      case poly: Polygon =>
        val holesArray = Array.tabulate(poly.getNumInteriorRing)(i => poly.getInteriorRingN(i))
        holesArray.toList
      case m: GeometryCollection =>
        val components = Array.tabulate(m.getNumGeometries)(i => m.getGeometryN(i)).toList
        components.flatMap(extractHoles)
    }
  }

  // ///////////////////////////////////////////////////////

  def centroidsOfFreeSpacesIn(s:Iterable[Geometry], outer:Geometry):Iterable[(Int,Int)] = {
    val frees = freeSpacesIn(s:Iterable[Geometry], outer:Geometry):Iterable[Geometry]
    val centroids = frees.map(_.getCentroid)
    centroids.map(centroid => (centroid.getX.toInt,centroid.getY.toInt))
  }

  def freeSpacesIn(s:Iterable[Geometry], outer:Geometry):Iterable[Geometry] = {
    var acc = outer
    var t = s

    while(t.nonEmpty){
      acc = acc difference t.head
      t = t.tail
    }

    extractShapes(acc)
  }
  private def extractShapes(g:Geometry):List[Geometry] = {
    g match {
      case poly: Polygon =>
        List(poly)
      case m: GeometryCollection =>
        val components = Array.tabulate(m.getNumGeometries)(i => m.getGeometryN(i)).toList
        components.flatMap(extractShapes)
    }
  }

  def centersOfFreeSpaces(s:Iterable[Geometry], outer:Geometry, nbSteps1D:Int):Iterable[(Long,Long)] = {
    var acc = outer
    var t = s

    while(t.nonEmpty){
      acc = acc difference t.head
      t = t.tail
    }

    val zone = acc
    val indexedZone = PreparedGeometryFactory.prepare(zone)
    //we search for point within zone o a grid such that no immediate neighbors on the grid ius closer to the shape.

    val boundingBox = zone.getEnvelope()
    val coordinates = boundingBox.getCoordinates()
    val minX = coordinates(0).x.toLong
    val minY = coordinates(0).y.toLong
    val maxX = coordinates(2).x.toLong
    val maxY = coordinates(2).y.toLong

    val stepX = (maxX - minX) / nbSteps1D
    val stepY = (maxY - minY) / nbSteps1D

    val xs = Array.tabulate(nbSteps1D+1)(xid => minX + stepX * xid)
    val ys = Array.tabulate(nbSteps1D+1)(yid => minY + stepY * yid)

    val distancesArray:Array[Array[Long]] = Array.tabulate(nbSteps1D+1)(_ => Array.fill(nbSteps1D+1)(-1L))

    for(xId <- 0 to nbSteps1D){
      for(yId <- 0 to nbSteps1D){
        val pt = geometry.point(xs(xId),ys(yId))
        if(indexedZone.contains(pt)){
          //it contains it :-)
          distancesArray(xId)(yId) = computeDistance(zone,pt).toLong
        }
      }
    }

    for(xId <- 0 to nbSteps1D){
      for(yId <- 0 to nbSteps1D){
        val valueXm1 = if(xId > 0) distancesArray(xId-1)(yId) else -1
        val valueXp1 = if(xId < nbSteps1D) distancesArray(xId+1)(yId) else -1
        val valueYm1 = if(yId > 0) distancesArray(xId)(yId-1) else -1
        val valueYp1 = if(yId < nbSteps1D) distancesArray(xId)(yId+1) else -1

        val biggestNeighbor:Long = List(valueXm1,valueXp1,valueYm1,valueYp1).max
        if(distancesArray(xId)(yId) < biggestNeighbor) {
          distancesArray(xId)(yId) = -1
        }
      }
    }

    xs.indices.flatMap(xId =>
      ys.indices.flatMap(yId =>
        if (distancesArray(xId)(yId) != -1) {
          Some(xs(xId), ys(yId))
        } else None
      )
    )
  }

  private def computeDistance(shape1:Geometry,point:Point):Double = {
    val ptDist = new PointPairDistance()
    DistanceToPoint.computeDistance(shape1,point.getCoordinate,ptDist) //in distance among all points of the geometry and other point
    ptDist.getDistance()
  }
}


/**
  * @param shapes
  * @param preComputeAll forces pre-computation of indexes for all shapes. use this if there are constant shapes that will never move/change
  */
class NoOverlapPenetration(shapes:Array[AtomicValue[GeometryValue]], preComputeAll:Boolean = true)
  extends Invariant with GeometryNotificationTarget{

  registerStaticAndDynamicDependencyArrayIndex(shapes)
  finishInitialization()

  val output = CBLSIntVar(model,name="violation of NoOverlapPenetration")
  output.setDefiningInvariant(this)

  //index in array => index in geometry, for fast overlap computation
  private var geometryIndexes:Array[PreparedGeometry] = Array.fill(shapes.size)(null)

  if(preComputeAll){
    for(id <- shapes.indices) computeAndReturnIndex(id)
  }

  private var changedShapesToCheck = IterableMagicBoolArray(shapes.size,initVal = true)

  private def computeAndReturnIndex(id:Int):PreparedGeometry = {
    geometryIndexes(id) = PreparedGeometryFactory.prepare(shapes(id).value.geometry)
    geometryIndexes(id)
  }

  private def returnIndexComputeIfNeeded(id:Int):PreparedGeometry = {
    if(geometryIndexes(id)!= null) geometryIndexes(id)
    else computeAndReturnIndex(id)
  }

  //we initialize as zero overlap, but all shapes are notes as having moved, and we schedule ourself for propagation.
  private val recordedOverlap = Array.tabulate(shapes.size)(id => Array.fill(id)(0L))
  output := 0


  //we initialize as zero overlap, but all shcpes are notes as having moved, and we schedule ourself for propagation.
  private val overlapByShape:Array[CBLSIntVar] = Array.tabulate(shapes.size)(id => {
    val v = CBLSIntVar(model,name="violation of overlap of shape " + id)
    v.setDefiningInvariant(this)
    v
  })

  def violation(shapeID:Int):IntValue = overlapByShape(shapeID)

  scheduleForPropagation()

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue],
                                    id: Int,
                                    oldVal: GeometryValue,
                                    newVal: GeometryValue): Unit = {
    changedShapesToCheck(id) = true
    geometryIndexes(id) = null
    scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    for(shapeIDToCheck <- changedShapesToCheck.indicesAtTrue){
      for(otherID <- shapes.indices if !changedShapesToCheck(otherID) || otherID < shapeIDToCheck){
        val newOverlap = computeOverlapViolation(shapes(shapeIDToCheck).value,shapeIDToCheck,shapes(otherID).value,otherID)
        val oldOverlap = recordedOverlap(shapeIDToCheck max otherID)(shapeIDToCheck min otherID)
        recordedOverlap(shapeIDToCheck max otherID)(shapeIDToCheck min otherID) = newOverlap
        output :+= (newOverlap - oldOverlap)
        overlapByShape(shapeIDToCheck) :+= (newOverlap - oldOverlap)
        overlapByShape(otherID) :+= (newOverlap - oldOverlap)
      }
    }
    changedShapesToCheck.all = false
  }

  private def computeOverlapViolation(shape1:GeometryValue,id1:Int,shape2:GeometryValue,id2:Int):Long = {
    if(! (shape1 mightOverlapBasedOnOverApproximatingValues shape2)) return 0

    if(geometryIndexes(id1) != null){
      if(geometryIndexes(id1).disjoint(shape2.geometry)) return 0
    }else if (geometryIndexes(id2) != null){
      if(geometryIndexes(id2).disjoint(shape1.geometry)) return 0
    }else{
      //create an index for the smallest shape
      if(shape1.geometry.getNumPoints < shape2.geometry.getNumPoints){
        if(computeAndReturnIndex(id1).disjoint(shape2.geometry)) return 0
      }else{
        if(computeAndReturnIndex(id2).disjoint(shape1.geometry)) return 0
      }
    }

    //There is an overlap, so we quantify it; we measure some penetration,
    //which is faster to compute than the overlap area
    //also it must be symmetric, and muse be able to capture improvement by rotation (that's why it must be symmetric?)
    /*
    (shape1.overApproximatingRadius
      + shape2.overApproximatingRadius -
      (computeDistance(shape1.geometry,shape2.centerOfOverApproximatingCircle)
        + computeDistance(shape2.geometry,shape1.centerOfOverApproximatingCircle))).toLong
*/
    val v = (shape1.overApproximatingRadius
      + shape2.overApproximatingRadius -
      (computeDistanceNegIfInside(shape1.geometry,id1,shape2.centerOfOverApproximatingCircle)
        + computeDistanceNegIfInside(shape2.geometry,id2,shape1.centerOfOverApproximatingCircle))).toLong

    v*v
  }

  private def computeDistanceNegIfInside(shape1:Geometry,id1:Int,point:Point):Double = {

    val containsPt = if(geometryIndexes(id1) == null){
      shape1.contains(point)
    }else{
      geometryIndexes(id1).contains(point)
    }

    val d = computeDistance(shape1:Geometry,point:Point)

    if(containsPt) -d else d
  }

  private def computeDistance(shape1:Geometry,point:Point):Double = {
    val ptDist = new PointPairDistance()
    DistanceToPoint.computeDistance(shape1,point.getCoordinate,ptDist) //in distance among all points of the geometry and other point
    ptDist.getDistance()
  }
}



