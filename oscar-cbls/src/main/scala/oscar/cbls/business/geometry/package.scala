package oscar.cbls.business

import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory}
import org.locationtech.jts.geom.util.AffineTransformation

package object geometry {


  //this is the general factory to use, it is kept in floats because putting it to integers
  // only adds an extra rounding at the end of each operation.
  //so better to keep it all in floats, and perform any conversion when geting things out of JTS
  val factory:GeometryFactory = new GeometryFactory()

  val emptyPolygon = factory.createMultiPolygon(Array.empty)

  def createSquare(side:Double):Geometry = {
    val halfSide = side/2

    geometry.factory.createLinearRing(Array(
      new Coordinate(halfSide,halfSide),
      new Coordinate(halfSide,-halfSide),
      new Coordinate(-halfSide,-halfSide),
      new Coordinate(-halfSide,halfSide),
      new Coordinate(halfSide,halfSide))).convexHull()
  }


  def createCircle(r:Double,nbEdges:Int = 16, ensureCorrectSurface:Boolean = true):Geometry = {
    println("creating circle ")
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
