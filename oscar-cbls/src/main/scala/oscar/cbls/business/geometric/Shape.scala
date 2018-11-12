package oscar.cbls.business.geometric

import java.awt.geom.Rectangle2D

import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualCircle, VisualRectangle, VisualShape}

abstract class Shape {
  def minX:Int
  def maxX:Int
  def minY:Int
  def maxY:Int

  val bbX = (maxX + minX)/2
  val bbY = (maxY + minY)/2
  val bbWidth = maxX - minX
  val bbHeight = maxY - minY

  def toVisualShape(xyMultiplier:Double,drawing:VisualDrawing): VisualShape
}

class Rectangle(val centreX:Int,val centreY:Int,val width:Int,val height:Int) extends Shape{

  val minX:Int = centreX - width/2
  val maxX:Int = centreX + width/2
  val minY:Int = centreY - height/2
  val maxY:Int = centreY + height/2

  override def toVisualShape(xyMultiplier:Double,drawing:VisualDrawing): VisualShape =
    new VisualRectangle(
      drawing,
      new Rectangle2D.Double(
        minX * xyMultiplier,
        minY * xyMultiplier,
        width * xyMultiplier,
        height * xyMultiplier))

  def isInside(x:Int,y:Int):Boolean = {
    minX <= x && x <= maxX && minY <= y && y <= maxY
  }
}

class Circle(val centreX:Int,val centreY:Int,val radius:Int) extends Shape{
  val minX:Int = centreX - radius
  val maxX:Int = centreX + radius
  val minY:Int = centreY - radius
  val maxY:Int = centreY + radius

  override def toVisualShape(xyMultiplier:Double,drawing:VisualDrawing): VisualShape =
    new VisualCircle(drawing,
      centreX * xyMultiplier,
      centreY * xyMultiplier,
      radius * xyMultiplier)

  def interectLine(x1:Int,y1:Int,x2:Int,y2:Int):Boolean = ???
}

object Shape{
  def isOverlapInBoundingBox(shape1:Shape,shape2:Shape):Boolean = {
    (math.abs(shape1.bbX - shape2.bbX) * 2 < (shape1.bbWidth + shape2.bbWidth)) &&
      (math.abs(shape1.bbY - shape2.bbY) * 2 < (shape1.bbHeight + shape2.bbHeight))
  }

  def isOverlap(shape1:Shape,shape2:Shape):Boolean = {
    (shape1,shape2) match{
      case (r1:Rectangle,r2:Rectangle) =>
        isOverlapInBoundingBox(shape1:Shape,shape2:Shape)
      case (c:Circle,r:Rectangle) =>
        r.isInside(c.centreX,c.centreY) || ???
      case (r:Rectangle,c:Circle) =>
        isOverlap(c,r)
      case (c1:Circle,c2:Circle) =>
        ((c1.centreX - c2.centreX)^2) + ((c1.centreY - c2.centreY)^2) < ((c1.radius + c2.radius)^2)
    }
  }

  def closestPoint(x1:Int,x2:Int,xl1:Int,yl1:Int,xl2:Int,yl2:Int):(Int,Int) = ???
}

