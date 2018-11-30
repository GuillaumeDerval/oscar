package oscar.cbls.business.geometry.visu

import java.awt.geom.Rectangle2D
import java.awt.{Color, Shape}

import org.locationtech.jts.awt.ShapeWriter
import org.locationtech.jts.geom._
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

    repaint()
  }

  def paintSomeHoles(s:List[Geometry]): Unit ={
    val w = new ShapeWriter()

    val hh = outerHoles(s.tail, s.head).toArray

    val areas = hh.map(_.getArea)
    val maxArea = areas.max

    for(i <- hh.indices){
      val h = hh(i)
      val s = new VisualShapeConcrete(this, w.toShape(h))
      s.fill = true

      if(areas(i) == maxArea){
        s.innerCol = Color.RED
      }else{
        s.innerCol = Color.BLUE
      }

      s.toolTip = "hole_" + i
    }
  }

  def holes(s:List[Geometry]):List[Geometry] = {
    var acc = s.head
    var t = s.tail
    while(t.nonEmpty){
      acc = acc union t.head
      t = t.tail
    }
    extractHoles(acc)
  }

  def extractHoles(g:Geometry):List[Geometry] = {
    g match {
      case poly: Polygon =>
        val holesArray = Array.tabulate(poly.getNumInteriorRing)(i => poly.getInteriorRingN(i))
        holesArray.toList
      case m: GeometryCollection =>
        val components = Array.tabulate(m.getNumGeometries)(i => m.getGeometryN(i)).toList
        components.flatMap(extractHoles)
    }
  }
  def extractShapes(g:Geometry):List[Geometry] = {
    g match {
      case poly: Polygon =>
        List(poly)
      case m: GeometryCollection =>
        val components = Array.tabulate(m.getNumGeometries)(i => m.getGeometryN(i)).toList
        components.flatMap(extractShapes)
    }
  }

  def outerHoles(s:List[Geometry], outer:Geometry):List[Geometry] = {
    var acc = outer
    var t = s

    while(t.nonEmpty){
      acc = acc difference t.head
      t = t.tail
    }

    extractShapes(acc)
  }
}


