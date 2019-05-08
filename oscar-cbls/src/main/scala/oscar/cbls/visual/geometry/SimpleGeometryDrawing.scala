
package oscar.cbls.visual.geometry

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

import java.awt.geom.{Line2D, Rectangle2D}
import java.awt.{Color, Shape}

import org.locationtech.jts.awt.ShapeWriter
import org.locationtech.jts.geom._
import org.locationtech.jts.geom.util.{AffineTransformation, GeometryTransformer}
import oscar.cbls.business.geometry.invariants.Overlap
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualArrow, VisualLine, VisualRectangle, VisualShape}


class SimpleGeometryDrawing(relevantDistances:List[(Int,Int)], windowWidth: Int = 960, windowHeight: Int = 960, pointShift: Option[() => (Double,Double)] = None)
  extends VisualDrawing(false,false) with GeometryDrawingTrait {

  class VisualShapeConcrete(d: VisualDrawing, s: Shape) extends VisualShape(d) {

    override def move(x: Double, y: Double): Unit = {}

    type S = Shape
    protected val shape = s
  }

  override def addShape(shape: VisualShape, repaintAfter: Boolean): Unit ={
    super.addShape(shape,false)
  }

  this.setSize(windowWidth, windowHeight)

  //TODO: il manque un zoom et un scroll!!!
  //TODO: on ne voit pas le border!!
  /**
    * @param shapes shape,bordercolor,innercolor,toltipText
    */
  def drawShapes(boundingBoxOn:Option[Geometry] = None, shapes:List[(Geometry,Option[Color],Option[Color],String)],centers:List[(Int,Int)]) ={
    super.clear(false)

    val w = new ShapeWriter()

    val (minX,maxX,minY,maxY) = boundingBoxOn match {
      case None =>
        (shapes.map(_._1.getCoordinates.map(c => c.x).min).min,
          shapes.map(_._1.getCoordinates.map(c => c.x).max).max,
          shapes.map(_._1.getCoordinates.map(c => c.y).min).min,
          shapes.map(_._1.getCoordinates.map(c => c.y).max).max)
      case Some(b) =>
        (b.getCoordinates.map(c => c.x).min,
          b.getCoordinates.map(c => c.x).max,
          b.getCoordinates.map(c => c.y).min,
          b.getCoordinates.map(c => c.y).max)
    }

    val drawingWidth = (maxX - minX).toInt
    val drawingHeight = (maxY - minY).toInt

    val (scaleOriginX,scaleOriginY) = pointShift.getOrElse(() => (0.0,0.0))()
    val scaling = Math.max((windowWidth.toDouble-(scaleOriginX*2))/drawingWidth,(windowHeight.toDouble-(2.0*scaleOriginY))/drawingHeight)
    val scaleTransform = AffineTransformation.scaleInstance(scaling,scaling)
    val translateTransform = AffineTransformation.translationInstance(scaleOriginX,scaleOriginY)

    val border =
      if(boundingBoxOn.isEmpty)
        new VisualRectangle(this, new Rectangle2D.Double(
          coordToPixel((minX.toInt,0), drawingWidth, drawingHeight)._1,
          coordToPixel((0,minY.toInt), drawingWidth, drawingHeight)._2,
          coordToPixel((drawingWidth,0), drawingWidth, drawingHeight)._1,
          coordToPixel((0,drawingHeight), drawingWidth, drawingHeight)._2))
      else
        new VisualShapeConcrete(this, w.toShape(translateTransform.transform(scaleTransform.transform(boundingBoxOn.get))))

    border.fill = false
    border.border = true
    border.borderWidth = 3
    border.outerCol = Color.black

    for((geometry,borderColOpt,innerColOpt,toolTipText) <- shapes){
      val s = new VisualShapeConcrete(this, w.toShape(translateTransform.transform(scaleTransform.transform(geometry))))

      borderColOpt match{
        case None =>
          s.border = false
        case Some(c) =>
          s.border = true
          s.outerCol = c
      }
      innerColOpt match{
        case None =>
          s.fill = false
        case Some(c) =>
          s.fill = true
          s.innerCol = c
      }

      s.toolTip = toolTipText
    }

    paintSomeHoles(shapes.map(_._1))

    for((fromID,toID) <- relevantDistances){
      val (x1,y1) = coordToPixel(centers(fromID), drawingWidth, drawingHeight)
      val (x2,y2) = coordToPixel(centers(toID), drawingWidth, drawingHeight)

      val line = new VisualArrow(this, new Line2D.Double(
        x1,
        y1,
        x2,
        y2),5)
      line.dashed = false
      line.innerCol = Color.BLUE
    }

    repaint()
  }

  def paintSomeHoles(s:List[Geometry]): Unit ={
    val w = new ShapeWriter()

    val hh = Overlap.freeSpacesIn(s.tail, s.head).toArray

    for(i <- hh.indices){
      val h = hh(i)
      val center = h.getCentroid
      val centroidPoint = new VisualShapeConcrete(this, w.toShape(center))
      centroidPoint.fill = true
      centroidPoint.innerCol = Color.BLACK
    }
  }

  private def coordToPixel(coord: (Int,Int), drawingWidth: Int, drawingHeight: Int): (Int,Int) ={
    val (shiftX,shiftY): (Double, Double)= if(pointShift.isDefined)pointShift.get.apply() else (0,0)
    val scaling = Math.max((windowWidth.toDouble-(shiftX*2))/drawingWidth,(windowHeight.toDouble-(shiftY*2))/drawingHeight)
    val x = ((coord._1.toDouble*scaling) + shiftX).toInt
    val y = ((coord._2.toDouble*scaling) + shiftY).toInt
    (x,y)
  }

}


