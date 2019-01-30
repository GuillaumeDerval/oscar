package oscar.cbls.business.geometric


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


import org.locationtech.jts.geom._
import org.locationtech.jts.geom.util.AffineTransformation
import org.locationtech.jts.io.WKTReader
import oscar.cbls.business.geometry
import oscar.cbls.business.geometry.visu.GeometryDrawing
import oscar.cbls.visual.{ColorGenerator, SingleFrameWindow}

object TesterBasic extends App{

  val rdr = new WKTReader
  val line1 = rdr.read("LINESTRING (0 0, 10 10, 20 20)").asInstanceOf[LineString]
  val line2 = rdr.read("LINESTRING (0 40, 60 40, 60 0, 20 0, 20 60)").asInstanceOf[LineString]

  for(nbEdges <- 4 to 100){
    val c = geometry.createCircle(1,nbEdges).getArea
    println("nbEdges:" + nbEdges + " area:" + c)
  }
  val rotation = AffineTransformation.rotationInstance(0.5)
  System.out.println("rotation:" + rotation)
  val translation = AffineTransformation.translationInstance(100, 100)
  System.out.println("translation:" + translation)
  val c = translation //new AffineTransformation(a).compose(b)
  //val c = new AffineTransformation(b).compose(a)

  System.out.println("composed:" + c)
  System.out.println("rotation:" + rotation)
  System.out.println("translation:" + translation)

  val polygon = line2.convexHull()
  val polygon2 = translation.transform(polygon)
  val polygon3 = translation.transform(polygon2)
  val polygon4 = translation.transform(polygon3)
  val polygon5 = rotation.transform(translation.transform(polygon4))
  val polygon6 = polygon.union(polygon5).union(polygon2).convexHull()
  val polygon7 = rotation.transform(translation.transform(translation.transform(polygon5.union(polygon4).convexHull().union(polygon6))))
  val polygon8 = translation.transform(polygon7.intersection(polygon6))

  val circle1 = AffineTransformation.translationInstance(300, 100).transform(geometry.createCircle(100,16))

  println("area of polygon8:" + polygon8.getArea)

  val allPolygons = List(
    polygon6,
    polygon7,
    polygon,
    polygon2,
    polygon8,
    polygon3,
    polygon4,
    polygon5,
    circle1)

  val drawing = new GeometryDrawing(List.empty)

  val randomColors = ColorGenerator.generateRandomColors(allPolygons.size,175).toIterator
  drawing.drawShapes(shapes = allPolygons.map(shape => (shape,None,Some(randomColors.next),"area:" + shape.getArea)),centers = List.empty)

  SingleFrameWindow.show(drawing,"a drawing with the standard oscar drawing console")

}
