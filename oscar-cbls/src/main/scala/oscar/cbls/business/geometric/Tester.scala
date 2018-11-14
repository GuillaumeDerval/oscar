package oscar.cbls.business.geometric

import java.awt.{BorderLayout, Color, Polygon, Shape}
import java.awt.geom.Rectangle2D
import java.util

import javax.swing.JFrame
import org.locationtech.jts.awt.{PolygonShape, ShapeWriter}
import org.locationtech.jts.geom._
import org.locationtech.jts.geom.util.{AffineTransformation, PolygonExtracter}
import org.locationtech.jts.io.WKTReader
import oscar.cbls.business.geometric.Tester.polygon6
import oscar.cbls.visual.ColorGenerator
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualPolygon, VisualRectangle, VisualShape}




class VisualShapeConcrete(d: VisualDrawing, s: Shape) extends VisualShape(d) {

  type S = Shape
  protected val shape = s

  override def move(x: Double, y: Double): Unit = {}
}


class Visu()
  extends VisualDrawing(false,false) {

  override def addShape(shape: VisualShape, repaintAfter: Boolean): Unit ={
    super.addShape(shape,false)
  }

  def drawShapes(shapes:List[Geometry]) ={

    val maxX = shapes.map(_.getCoordinates.map(c => c.x ).max).max
    val maxY = shapes.map(_.getCoordinates.map(c => c.y).max).max

    val xyMultiplier = ((this.getWidth.toDouble min this.getHeight.toDouble)-10)/(maxX max maxY)
    super.clear(false)

    val border = new VisualRectangle(this, new Rectangle2D.Double(
      0,
      0,
      maxX*xyMultiplier,
      maxY*xyMultiplier))

    border.fill = false
    border.border = true
    border.outerCol = Color.black

    val colors = ColorGenerator.generateRandomColors(shapes.length).toIterator

    val w = new ShapeWriter()
    for(shape <- shapes){
      val s = new VisualShapeConcrete(this, w.toShape(shape))
      s.innerCol = colors.next
      s.toolTip = "area:" + shape.getArea
    }

    repaint()
  }
}


class GeoVisuWindow(){

  val visual = new Visu()

  val frame = new JFrame()
  frame.setTitle("JTS polygon displayed in oscar.visu, called from oscar in scala")
  frame.setLayout(new BorderLayout())
  frame.setPreferredSize(new java.awt.Dimension(960,960))
  frame.add(visual, BorderLayout.CENTER)
  frame.pack()
  frame.revalidate()
  frame.setVisible(true)
}

object Tester extends App{

  val rdr = new WKTReader
  val line1 = rdr.read("LINESTRING (0 0, 10 10, 20 20)").asInstanceOf[LineString]
  showSelfIntersections(line1)
  val line2 = rdr.read("LINESTRING (0 40, 60 40, 60 0, 20 0, 20 60)").asInstanceOf[LineString]
  showSelfIntersections(line2)

  def createCircle(r:Double,nbEdges:Int = 16, ensureCorrectSurface:Boolean = true,factory:GeometryFactory = new GeometryFactory()):Geometry = {
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


  for(nbEdges <- 4 to 100){
    val c = createCircle(1,nbEdges).getArea
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

  val v = new GeoVisuWindow()

  val polygon = line2.convexHull()
  val polygon2 = translation.transform(polygon)
  val polygon3 = translation.transform(polygon2)
  val polygon4 = translation.transform(polygon3)
  val polygon5 = rotation.transform(translation.transform(polygon4))
  val polygon6 = polygon.union(polygon5).union(polygon2).convexHull()
  val polygon7 = rotation.transform(translation.transform(translation.transform(polygon5.union(polygon4).convexHull().union(polygon6))))
  val polygon8 = translation.transform(polygon7.intersection(polygon6))

  val circle1 = AffineTransformation.translationInstance(300, 100).transform(createCircle(100,16))

  println("area of polygon8:" + polygon8.getArea)


  v.visual.drawShapes(List(
    polygon6,
    polygon7,
    polygon,
    polygon2,
    polygon8,
    polygon3,
    polygon4,
    polygon5,
    circle1))



  def showSelfIntersections(line: LineString): Unit = {
    System.out.println("Line: " + line)
    System.out.println("Self Intersections: " + lineStringSelfIntersections(line))
  }

  def lineStringSelfIntersections(line: LineString): Geometry = {
    val lineEndPts = getEndPoints(line)
    val nodedLine = line.union(lineEndPts)
    val nodedEndPts = getEndPoints(nodedLine)
    val selfIntersections = nodedEndPts.difference(lineEndPts)
    selfIntersections
  }

  def getEndPoints(g: Geometry): Geometry = {
    val endPtList = new util.ArrayList[Coordinate]
    if (g.isInstanceOf[LineString]) {
      val line = g.asInstanceOf[LineString]
      endPtList.add(line.getCoordinateN(0))
      endPtList.add(line.getCoordinateN(line.getNumPoints - 1))
    }
    else if (g.isInstanceOf[MultiLineString]) {
      val mls = g.asInstanceOf[MultiLineString]
      var i = 0
      while ( {
        i < mls.getNumGeometries
      }) {
        val line = mls.getGeometryN(i).asInstanceOf[LineString]
        endPtList.add(line.getCoordinateN(0))
        endPtList.add(line.getCoordinateN(line.getNumPoints - 1))

        {
          i += 1; i - 1
        }
      }
    }
    val endPts = CoordinateArrays.toCoordinateArray(endPtList)
    (new GeometryFactory).createMultiPoint(endPts)
  }

}
