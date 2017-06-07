package oscar.cbls.visual.wlp

import java.awt.geom.Rectangle2D
import java.awt.{Dimension, BorderLayout, Color}
import java.awt.geom.Line2D.Double
import javax.swing.JFrame

import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualRectangle, VisualCircle, VisualLine, VisualShape}

import scala.collection.immutable.SortedSet

class WareHouseLocationWindow(deliveryCoordinates:Array[(Int,Int)],
                              wareHouseCoordinates:Array[(Int,Int)],
                              distanceCostD2W:Array[Array[Int]],
                              warehouseCosts:Array[Int]){

  val visual = new WareHouseLocationMap(deliveryCoordinates,wareHouseCoordinates,distanceCostD2W,warehouseCosts)

  def redraw(openWarehouses:SortedSet[Int]){
    visual.redraw(openWarehouses)
  }
  val frame = new JFrame()
  frame.setTitle("Uncapacitated Warehouse Location Problem")
  frame.setLayout(new BorderLayout())
  frame.setPreferredSize(new Dimension(960,960))
  frame.add(visual, BorderLayout.CENTER)
  frame.pack()
  frame.revalidate()
  frame.setVisible(true)
}

class WareHouseLocationMap(deliveryCoordinates:Array[(Int,Int)],
                           wareHouseCoordinates:Array[(Int,Int)],
                           distanceCostD2W:Array[Array[Int]],
                           warehouseCosts:Array[Int])
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

    val xMultiplier = this.getWidth.toDouble / maxX.toDouble
    val yMultiplier = this.getHeight.toDouble / maxY.toDouble

    super.clear()

    for(delivery <- 0 until d){
      val warehouse = closestWarehouses(delivery)
      if(warehouse != -1){
        val line = new VisualLine(this,new Double(
          deliveryCoordinates(delivery)._1 * xMultiplier,
          deliveryCoordinates(delivery)._2 * yMultiplier,
          wareHouseCoordinates(warehouse)._1 * xMultiplier,
          wareHouseCoordinates(warehouse)._2 * yMultiplier))
        line.dashed = true
      }
    }

    def drawWarehouse(warehouse:Int,color:Color){
      val tempPoint = new VisualRectangle(this, new Rectangle2D.Double(
        wareHouseCoordinates(warehouse)._1 * xMultiplier - 4,
        wareHouseCoordinates(warehouse)._2 * yMultiplier - 4,
        8,
        8))
      tempPoint.innerCol_$eq(color)
      tempPoint.toolTip = "warehouseCost:" + warehouseCosts(warehouse)
    }

    for(warehouse <- 0 until w if !(openWarehouses contains warehouse)) {
      drawWarehouse(warehouse, Color.PINK)
    }
    for(warehouse <- 0 until w if openWarehouses contains warehouse) {
      drawWarehouse(warehouse, Color.green)
    }

    for(delivery <- 0 until d){
      val warehouse = closestWarehouses(delivery)
      val color =
        if(warehouse == -1)Color.red
        else Color.black
      val tempPoint = new VisualCircle(this,
        deliveryCoordinates(delivery)._1 * xMultiplier,
        deliveryCoordinates(delivery)._2 * yMultiplier,2)
      tempPoint.innerCol_$eq(color)

      if(warehouse != -1){
        tempPoint.toolTip = "distanceCost:" + distanceCostD2W(delivery)(warehouse)
      }
    }

    repaint()
  }
}
