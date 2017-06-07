package oscar.cbls.visual.wlp

import java.awt.{Dimension, BorderLayout, Color}
import java.awt.geom.Line2D.Double
import javax.swing.JFrame

import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualCircle, VisualLine, VisualShape}

import scala.collection.immutable.SortedSet


class WareHouseLocationWindow(deliveryCoordinates:Array[(Int,Int)],
                              wareHouseCoordinates:Array[(Int,Int)],
                              distanceCostD2W:Array[Array[Int]]){

  val visual = new WareHouseLocationMap(deliveryCoordinates,wareHouseCoordinates,distanceCostD2W)

  def redraw(openWarehouses:SortedSet[Int]){
    visual.redraw(openWarehouses)
  }
  val frame = new JFrame()
  frame.setLayout(new BorderLayout())
  frame.setPreferredSize(new Dimension(960,960))
  frame.add(visual, BorderLayout.CENTER)
  frame.pack()
  frame.revalidate()
  frame.setVisible(true)

}



class WareHouseLocationMap(deliveryCoordinates:Array[(Int,Int)],
                           wareHouseCoordinates:Array[(Int,Int)],
                           distanceCostD2W:Array[Array[Int]])
  extends VisualDrawing(false,false){

  val maxX = math.max(deliveryCoordinates.map(_._1).max,wareHouseCoordinates.map(_._1).max)
  val maxY = math.max(deliveryCoordinates.map(_._2).max,wareHouseCoordinates.map(_._2).max)

  val w = wareHouseCoordinates.length
  val d = deliveryCoordinates.length

  override def addShape(shape: VisualShape, repaintAfter: Boolean = true): Unit ={
    super.addShape(shape,false)
  }

  def redraw(openWarehouses:SortedSet[Int]){
    val closestWarehouses:Array[Int] = Array.tabulate(d)(nearestOpenWareHouse(openWarehouses,_))
    drawMap(closestWarehouses,openWarehouses)
  }

  /**
   * @param d
   * @return -1 is no open warehouse
   */
  private def nearestOpenWareHouse(openWarehouses:SortedSet[Int],d:Int):Int = {
    var closestW = -1
    var minDistance = Int.MaxValue
    for(w <- openWarehouses){
      val distance = distanceCostD2W(d)(w)
      if(distance < minDistance){
        closestW = w
        minDistance = distance
      }
    }
    closestW
  }

  private def drawMap(closestWarehouses:Array[Int],openWarehouses:SortedSet[Int]) ={


    val xMultiplier = (this.getWidth / maxX.toDouble).toInt
    val yMultiplier = (this.getHeight / maxY.toDouble).toInt

    super.clear()

    for(delivery <- 0 until d){
      val warehouse = closestWarehouses(delivery)
      val color =
        if(warehouse == -1)Color.red
        else Color.black
      val tempPoint = new VisualCircle(this,
        deliveryCoordinates(delivery)._1 * xMultiplier,
        deliveryCoordinates(delivery)._2 * yMultiplier,1)
      tempPoint.innerCol_$eq(color)


      if(warehouse != -1){
        new VisualLine(this,new Double(
          deliveryCoordinates(delivery)._1 * xMultiplier,
          deliveryCoordinates(delivery)._2 * yMultiplier,
          wareHouseCoordinates(warehouse)._1 * xMultiplier,
          wareHouseCoordinates(warehouse)._2 * yMultiplier))
      }
    }

    for(warehouse <- 0 until w){
      val color =
        if(openWarehouses contains warehouse)Color.green
        else Color.red
      val tempPoint = new VisualCircle(this,
        wareHouseCoordinates(warehouse)._1 * xMultiplier,
        wareHouseCoordinates(warehouse)._2 * yMultiplier,4)
      tempPoint.innerCol_$eq(color)
    }

    repaint()
  }
}
