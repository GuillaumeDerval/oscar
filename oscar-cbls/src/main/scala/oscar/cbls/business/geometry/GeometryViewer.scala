package oscar.cbls.business.geometry

import java.awt.geom.Rectangle2D
import java.awt.{Color, Shape}

import org.locationtech.jts.awt.ShapeWriter
import org.locationtech.jts.geom.Geometry
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualRectangle, VisualShape}


class GeometryDrawing()
  extends VisualDrawing(false,false) {

  class VisualShapeConcrete(d: VisualDrawing, s: Shape) extends VisualShape(d) {

    type S = Shape
    protected val shape = s

    override def move(x: Double, y: Double): Unit = {}
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
  def drawShapes(boundingBoxOn:Option[Geometry] = None,shapes:List[(Geometry,Option[Color],Option[Color],String)]) ={

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

    val xyMultiplier = (this.getWidth.toDouble min this.getHeight.toDouble)/(width max height)

    super.clear(false)

    val border = new VisualRectangle(this, new Rectangle2D.Double(
      minX*xyMultiplier,
      minY*xyMultiplier,
      width*xyMultiplier,
      height*xyMultiplier))

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

    repaint()
  }
}


