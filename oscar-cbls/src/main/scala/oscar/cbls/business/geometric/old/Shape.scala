package oscar.cbls.business.geometric.old

import java.awt.geom.{Line2D, Rectangle2D}
import java.awt.{BorderLayout, Color, Dimension}

import javax.swing.JFrame
import oscar.cbls.visual.ColorGenerator
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualCircle, VisualLine, VisualRectangle, VisualShape}

abstract sealed class Shape {
  def minX:Int
  def maxX:Int
  def minY:Int
  def maxY:Int

  def bbX = (maxX + minX)/2
  def bbY = (maxY + minY)/2
  def bbWidth = maxX - minX
  def bbHeight = maxY - minY

  def toVisualShape(xyMultiplier:Double,drawing:VisualDrawing): VisualShape
}

case class Rectangle(val centreX:Int,val centreY:Int,val width:Int,val height:Int) extends Shape{

  override val minX:Int = centreX - (width/2)
  override val maxX:Int = centreX + (width/2)
  override val minY:Int = centreY - (height/2)
  override val maxY:Int = centreY + (height/2)
  override val bbX = centreX
  override val bbY = centreY
  override val bbWidth = width
  override val bbHeight = height

  override def toVisualShape(xyMultiplier:Double,drawing:VisualDrawing): VisualShape =
    new VisualRectangle(
      drawing,
      new Rectangle2D.Double(
        minX * xyMultiplier,
        minY * xyMultiplier,
        width * xyMultiplier,
        height * xyMultiplier))
}

case class Circle(val centreX:Int,val centreY:Int,val radius:Int) extends Shape{
  override val minX:Int = centreX - radius
  override val maxX:Int = centreX + radius
  override val minY:Int = centreY - radius
  override val maxY:Int = centreY + radius
  override val bbX = centreX
  override val bbY = centreY
  override val bbWidth = radius*2
  override val bbHeight = radius*2

  override def toVisualShape(xyMultiplier:Double,drawing:VisualDrawing): VisualShape =
    new VisualCircle(drawing,
      centreX * xyMultiplier,
      centreY * xyMultiplier,
      radius * xyMultiplier)
}

object Shape{

  def p2(x:Int):Int = x*x

  def isOverlap(shape1:Shape,shape2:Shape):Boolean = {
    (shape1,shape2) match{
      case (r1:Rectangle,r2:Rectangle) =>
        ((math.abs(r1.centreX - r2.centreX) * 2) < (r1.bbWidth + r2.bbWidth)) &&
          ((math.abs(r1.centreY - r2.centreY) * 2) < (r1.bbHeight + r2.bbHeight))
      case (c:Circle,r:Rectangle) =>
        //horizontal rect
        (((math.abs(c.centreX - r.centreX)*2 < (2*c.radius + r.width)) &&
          (math.abs(c.centreY - r.centreY)*2 <= r.height))
          //vertical vert
          || ((math.abs(c.centreX - r.centreX)*2 <= r.width) &&
          (math.abs(c.centreY - r.centreY)*2 < (c.radius*2 + r.height)))
          //four corners, rounded
          || (p2(c.centreX - r.maxX) + p2(c.centreY - r.maxY) < p2(c.radius))
          || (p2(c.centreX - r.maxX) + p2(c.centreY - r.minY) < p2(c.radius))
          || (p2(c.centreX - r.minX) + p2(c.centreY - r.maxY) < p2(c.radius))
          || (p2(c.centreX - r.minX) + p2(c.centreY - r.minY) < p2(c.radius)))
      case (r:Rectangle,c:Circle) =>
        isOverlap(c,r)
      case (c1:Circle,c2:Circle) =>
        (p2(c1.centreX - c2.centreX) + p2(c1.centreY - c2.centreY)) < p2(c1.radius + c2.radius)
    }
  }
}

class GeoVisu()
  extends VisualDrawing(false,false){

  override def addShape(shape: VisualShape, repaintAfter: Boolean): Unit ={
    super.addShape(shape,false)
  }

  def drawShapes(shapes:List[Shape]) ={

    val maxX = shapes.map(s => s.maxX).max
    val maxY = shapes.map(_.maxY).max

    val xyMultiplier = ((this.getWidth.toDouble min this.getHeight.toDouble)-10)/ (maxX.toDouble max maxY.toDouble)
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

    for(shape <- shapes){
      val s = shape.toVisualShape(xyMultiplier,this)
      s.innerCol = colors.next
      s.toolTip = shape.toString
    }

    val shapesA = shapes.toArray
    for(i <- shapes.indices){
      for(j <- 0 until i){
        val shape1 = shapesA(i)
        val shape2 = shapesA(j)

        if (Shape.isOverlap(shape1,shape2)) {
          val line = new VisualLine(this, new Line2D.Double(
            shape1.bbX * xyMultiplier,
            shape1.bbY * xyMultiplier,
            shape2.bbX * xyMultiplier,
            shape2.bbY * xyMultiplier
            ))
          line.outerCol = Color.red
          line.borderWidth = 4
        }
      }
    }

    repaint()
  }
}
class GeoVisuWindow(){

  val visual = new GeoVisu()

  val frame = new JFrame()
  frame.setTitle("Uncapacitated Warehouse Location Problem")
  frame.setLayout(new BorderLayout())
  frame.setPreferredSize(new Dimension(960,960))
  frame.add(visual, BorderLayout.CENTER)
  frame.pack()
  frame.revalidate()
  frame.setVisible(true)
}
object Test extends App{
  val c1 = new Circle(10,10,10)
  val c2 = new Circle(12,50,10)
  val c3 = new Circle(30,10,10)
  val c4 = new Circle(10,30,10)
  val r1 = new Rectangle(90,50,40,80)
  val r2 = new Rectangle(50,100,80,30)
  val r3 = new Rectangle(60,80,10,20)
  val r4 = new Rectangle(20,90,90,10)

  def check(shapes:List[Shape]): Unit ={
    println("checking:" + shapes)
    println("overlapByLineSweep:" + OverlapDetection.isOverlapByLineSweep(shapes))
    println("overlapNaive:" + OverlapDetection.isOverlapNaive(shapes))
    println("overlapNaiveRev:" + OverlapDetection.isOverlapNaive(shapes.reverse))

    val visuWindow = new GeoVisuWindow
    visuWindow.visual.drawShapes(shapes)
    visuWindow.frame.setTitle(shapes + "overlap:" + OverlapDetection.isOverlapByLineSweep(shapes))
    println
  }

  check(List(Rectangle(90,90,40,80), Rectangle(50,100,80,30)))

  check(List(c1,c2,c3,c4,r1,r2,r3,r4))
  check(List(c1,c2))
  check(List(c1,c3,r1,r2,r3))
  check(List(c1,c2,c3,c4,r1,r2,r3,r4))
}

