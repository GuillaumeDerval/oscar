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
import org.locationtech.jts.algorithm.locate.IndexedPointInAreaLocator
import org.locationtech.jts.geom.prep.{PreparedGeometry, PreparedGeometryFactory}
import org.locationtech.jts.geom.{Geometry, GeometryCollection, Point, Polygon}
import oscar.cbls.algo.magicArray.IterableMagicBoolArray
import oscar.cbls.business.geometry.model.{CBLSGeometryVar, GeometryNotificationTarget, GeometryValue}
import oscar.cbls.core.IntInvariant
import oscar.cbls.core.computation.ChangingAtomicValue

import scala.collection.immutable.SortedMap

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
  private def extractShapes(g:Geometry):List[Geometry] = {
    g match {
      case poly: Polygon =>
        List(poly)
      case m: GeometryCollection =>
        val components = Array.tabulate(m.getNumGeometries)(i => m.getGeometryN(i)).toList
        components.flatMap(extractShapes)
    }
  }
}


class NoOverlap(shapes:Array[CBLSGeometryVar])
  extends IntInvariant
    with GeometryNotificationTarget{

  registerStaticAndDynamicDependencyArrayIndex(shapes)
  finishInitialization()

  //index in array => index in geometry, for fast overlap computation
  var geometryIndexes:Array[PreparedGeometry] = Array.fill(shapes.size)(null)
  var changedShapesToCheck = IterableMagicBoolArray(shapes.size,initVal = true)
  scheduleForPropagation()

  val recordedOverlap = Array.tabulate(shapes.size)(id => Array.fill(id-1)(0))
  this := 0

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
      for(otherID <- 0 until shapeIDToCheck){
        val newOverlap = computeOverlapViolation(shapes(shapeIDToCheck).value,shapeIDToCheck,shapes(otherID).value,otherID)
        val oldOverlap = recordedOverlap(shapeIDToCheck)(otherID)
        recordedOverlap(shapeIDToCheck)(otherID) = newOverlap
        this :+= (newOverlap - oldOverlap)
      }
    }
    changedShapesToCheck.all = false
  }

  private def computeOverlapViolation(shape1:GeometryValue,id1:Int,shape2:GeometryValue,id2:Int):Int = {
    if(! (shape1 mightOverlapBasedOnOverApproximatingValues shape2)) return 0

    if(geometryIndexes(id1) != null){
      if(geometryIndexes(id1).disjoint(shape2.geometry)) return 0
    }else if (geometryIndexes(id2) != null){
      if(geometryIndexes(id2).disjoint(shape1.geometry)) return 0
    }else{
      //create an index for the biggest shape
      if(shape1.geometry.getNumPoints > shape2.geometry.getNumPoints){
        geometryIndexes(id1) = PreparedGeometryFactory.prepare(shape1.geometry)
      }else{
        geometryIndexes(id2) = PreparedGeometryFactory.prepare(shape2.geometry)
      }
    }

    //here, there is an overlap, so we quantify it, based on the penetration
    (shape1.overApproximatingRadius
    + shape1.overApproximatingRadius
    - computeDistance(shape1.geometry,shape2.centerOfOverApproximatingCircle)
    - computeDistance(shape2.geometry,shape1.centerOfOverApproximatingCircle)).toInt
  }

  private val ptDist = new PointPairDistance()
  private def computeDistance(shape1:Geometry,point:Point):Double = {
    DistanceToPoint.computeDistance(shape1,point.getCoordinate,ptDist)
    ptDist.getDistance()
  }
  //criterion of overlap = penetration of approximating circle of smallest indice shape by perimeter for largest shape
  //penetration(shape1,shape2) = penetrationAS(shape1,shape2) + penetrationAS(shape2,shape1)
  //penetrationAS(shape1,shape2) = shape1.radius - (minimalDistance(shape1.centre; shape2))
  //minimalDistance se alcule comment?? avec DistanceToPoint


}

