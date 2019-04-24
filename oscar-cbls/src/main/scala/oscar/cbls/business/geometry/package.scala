package oscar.cbls.business


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


import org.locationtech.jts.geom.util.AffineTransformation
import org.locationtech.jts.geom.{Coordinate, GeometryFactory, Point}
import oscar.cbls.business.geometry.model.GeometryValue

package object geometry {

  // This is the general factory to use, it is kept in floats because putting it to integers
  // only adds an extra rounding at the end of each operation.
  // so better to keep it all in floats, and perform any conversion when getting things out of JTS
  val factory:GeometryFactory = new GeometryFactory()

  val emptyPolygon = factory.createMultiPolygon(Array.empty)

  val emptyGeometryValue = new GeometryValue(emptyPolygon,
    likeCircle= true)(
    inputArea = Some(0),
    inputCentroid = None,
    inputOverApproximatingRadius = Some(0))

  def createSquare(side:Double):GeometryValue = {
    val halfSide = side/2

    val sq = geometry.factory.createLinearRing(Array(
      new Coordinate(halfSide,halfSide),
      new Coordinate(halfSide,-halfSide),
      new Coordinate(-halfSide,-halfSide),
      new Coordinate(-halfSide,halfSide),
      new Coordinate(halfSide,halfSide))).convexHull()

    new GeometryValue(sq,
      likeCircle = true)(
      inputArea = Some(side * side),
      inputCentroid = Some(new Point(new Coordinate(0,0))),
      inputOverApproximatingRadius = Some(halfSide * math.sqrt(2.0)),
      inputPerimeter = Some(4*side))
  }

  def createRectangle(height:Double,width:Double):GeometryValue = {

    val halfHeight = height /2
    val halfWidth = width/2

    var rec = geometry.factory.createLinearRing(Array(
      new Coordinate(halfWidth,halfHeight),
      new Coordinate(halfWidth,-halfHeight),
      new Coordinate(-halfWidth,-halfHeight),
      new Coordinate(-halfWidth,halfHeight),
      new Coordinate(halfWidth,halfHeight))).convexHull()

    new GeometryValue(rec,
      likeCircle = true)(
      inputArea = Some(height * width),
      inputCentroid = Some(new Point(new Coordinate(0,0))),
      inputOverApproximatingRadius = Some(math.sqrt((halfHeight * halfHeight) + (halfWidth * halfWidth))),
      inputPerimeter = Some(2*(height + width))
    )
  }

  def createCircle(r:Double,nbEdges:Int = 16, ensureCorrectSurface:Boolean = true):GeometryValue = {
    println("creating circle ")
    require(nbEdges >=4,"a circle is hard to approximate with less than four edges...")

    val pointRotation = AffineTransformation.rotationInstance((2*math.Pi)/nbEdges)

    val startPoint = new Coordinate(r, 0)
    var currentPoint:Coordinate = startPoint
    val allPoints:Array[Coordinate] = Array.fill(nbEdges+1)(null)
    allPoints(0) = currentPoint
    allPoints(nbEdges) = currentPoint

    for(currentIndice <- 1 until nbEdges){
      currentPoint = pointRotation.transform(currentPoint,new Coordinate(0, 0))
      allPoints(currentIndice) = currentPoint
    }

    val c1 = factory.createPolygon(allPoints)

    if(ensureCorrectSurface){
      val s1 = c1.getArea  //TODO: this is crappy slow; hopefully called only once ?
      val factor = math.sqrt(math.Pi*r*r/s1)
      val scaling = AffineTransformation.scaleInstance(factor,factor)

      new GeometryValue(scaling.transform(c1),
        likeCircle = true)(
        inputArea = Some(math.Pi*r*r),
        inputCentroid = Some(new Point(new Coordinate(0,0))),
        inputOverApproximatingRadius = Some(factor * r))

    }else{
      new GeometryValue(c1,
        likeCircle = true)(
        inputArea = None,
        inputCentroid = Some(new Point(new Coordinate(0,0))),
        inputOverApproximatingRadius = Some(r))
    }
  }
}
