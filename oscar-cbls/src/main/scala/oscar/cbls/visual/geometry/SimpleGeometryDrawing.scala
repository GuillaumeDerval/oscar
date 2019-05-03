
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
import oscar.cbls.business.geometry.invariants.Overlap
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualLine, VisualRectangle, VisualShape}


class SimpleGeometryDrawing(relevantDistances:List[(Int,Int)])
  extends VisualDrawing(false,false) with GeometryDrawingTrait {

  class VisualShapeConcrete(d: VisualDrawing, s: Shape) extends VisualShape(d) {

    override def move(x: Double, y: Double): Unit = {}

    type S = Shape
    protected val shape = s
  }

  override def addShape(shape: VisualShape, repaintAfter: Boolean): Unit ={
    super.addShape(shape,false)
  }

  //TODO: il manque un zoom et un scroll!!!
  //TODO: on ne voit pas le border!!
  /**
    *
    * @param boundingBoxOn
    * @param shapes shape,bordercolor,innercolor,toltipText
    */
  def drawShapes(boundingBoxOn:Option[Geometry] = None,shapes:List[(Geometry,Option[Color],Option[Color],String)],centers:List[(Int,Int)]) ={
    super.clear(false)

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

    val width = maxX - minX
    val height = maxY - minY

    val border = new VisualRectangle(this, new Rectangle2D.Double(
      minX,
      minY,
      width,
      height))

    border.fill = false
    border.border = true
    border.outerCol = Color.black

    val w = new ShapeWriter()
    for((geometry,borderColOpt,innerColOpt,toolTipText) <- shapes){
      val s = new VisualShapeConcrete(this, w.toShape(geometry))

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
      val (x1,y1) = centers(fromID)
      val (x2,y2) = centers(toID)

      val line = new VisualLine(this, new Line2D.Double(
        x1,
        y1,
        x2,
        y2))
      line.dashed = false
      line.innerCol = Color.BLUE
    }

    repaint()
  }

  def paintSomeHoles(s:List[Geometry]): Unit ={
    val w = new ShapeWriter()

    val hh = Overlap.freeSpacesIn(s.tail, s.head).toArray

    val areas = hh.map(_.getArea)
    val maxArea = areas.max

    val transparentGreen = new Color(0,255,0,100)
    for(i <- hh.indices){
      val h = hh(i)

      /*val s = new VisualShapeConcrete(this, w.toShape(h))
      s.fill = true

      s.innerCol = transparentGreen
      s.border = true
      s.outerCol = transparentGreen
            s.toolTip = "hole_" + i

      */
      /*
      if(areas(i) == maxArea){
        s.innerCol = Color.RED
      }else{
        s.innerCol = Color.BLUE
      }*/


      val center = h.getCentroid
      val centroidPoint = new VisualShapeConcrete(this, w.toShape(center))
      centroidPoint.fill = true
      centroidPoint.innerCol = Color.BLACK
    }
  }

}


