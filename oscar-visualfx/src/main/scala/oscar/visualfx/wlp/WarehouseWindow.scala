package oscar.visualfx.wlp

import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

import org.apache.commons.lang.time.StopWatch
import oscar.visualfx.VisualFrameScalaFX
import scalafx.application.Platform
import scalafx.scene.chart.XYChart.{Data, Series}
import scalafx.scene.chart.{NumberAxis, ScatterChart}
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Line

class WarehouseWindow(distanceCost: Array[Array[Int]], warehousePos: Array[(Int,Int)], deliveryPos: Array[(Int,Int)]) extends VisualFrameScalaFX("WLP") {

  val watch = new StopWatch
  var lastUpdate:Long = 1000
  val TPE = new ThreadPoolExecutor(1, 1, 60, TimeUnit.SECONDS, new LinkedBlockingQueue[Runnable])
  val distance: Array[Array[Int]] = distanceCost
  val offset = 50
  val Xmax: Int = List(warehousePos.map(_._1).max, deliveryPos.map(_._1).max).max + offset
  val Xmin: Int = List(warehousePos.map(_._1).min, deliveryPos.map(_._1).min).min - offset
  val Ymax: Int = List(warehousePos.map(_._2).max, deliveryPos.map(_._2).max).max + offset
  val Ymin: Int = List(warehousePos.map(_._2).min, deliveryPos.map(_._2).min).min - offset
  val scatterChart = new ScatterChart(new NumberAxis(Xmin, Xmax, 1), new NumberAxis(Ymin, Ymax, 1))
  val warehouse = Array.tabulate(warehousePos.length)(i => new WareHouse(this, warehousePos(i), i))
  val deliveryStores = Array.tabulate(deliveryPos.length)(i => new DeliveryStore(deliveryPos(i), i))
  val warehouseSeries = new Series[Number, Number] {
    name = "Closed Warehouse"
  }
  val deliverySeries = new Series[Number, Number] {
    name = "Delivery Stores"
  }
  val stackPane = new Pane()

  scatterChart.legendVisible = false
  scatterChart.animated = false
  scatterChart.setMinSize(900, 800)
  stackPane.children.addAll(scatterChart)
  this.setFrameNodes(node = stackPane)
  this.sizeToScene()
  this.showStage()


  warehouse.foreach(w => {
    val data = Data[Number, Number](w.pos._1, w.pos._2)
    w.setVisible(true)
    data.setNode(w)
    warehouseSeries.getData.add(data)
  })


  deliveryStores.foreach(d => {
    val data = Data[Number, Number](d.pos._1, d.pos._2)
    d.setVisible(true)
    data.setNode(d)
    deliverySeries.getData.add(data)
  })

  scatterChart.getData.addAll(warehouseSeries, deliverySeries)

  deliveryStores.foreach(d => addDeliveryToWarehouse(d, getNearestOpenWarehouse(d, distanceCost, init = true), false))

  def addline(startNode: WareHouse, endNode: DeliveryStore): Unit = {
    val startPoint = scatterChart.localToParent(startNode.localToParent(startNode.getBoundsInLocal))
    val endPoint = scatterChart.localToParent(endNode.localToParent(endNode.getBoundsInLocal))
    val line = new Line {
      fill = Color.Black
      stroke = Color.Black
      startX = startPoint.getMaxX + startPoint.getWidth
      startY = startPoint.getMaxY + startPoint.getHeight
      endX = endPoint.getMaxX + startPoint.getWidth
      endY = endPoint.getMaxY + startPoint.getHeight
      smooth = true
    }
    line.getStrokeDashArray.addAll(2d)
    Platform.runLater({
      this.stackPane.children.add(line)
      line.toBack()
    })
    startNode.connectorLines = startNode.connectorLines ++ Seq(line)
  }

  def removeLines(w: WareHouse): Unit = {
    w.connectorLines.foreach(f => Platform.runLater(this.stackPane.children.removeAll(f)))
    w.connectorLines = Seq()
  }

  def getNearestOpenWarehouse(d: DeliveryStore, distanceCost: Array[Array[Int]], init: Boolean = false): WareHouse = {
    var nearestWarehouse: WareHouse = null
    var distMin = Int.MaxValue
    for (i <- distanceCost(d._id).indices) {
      val distance: Int = distanceCost(d._id)(i)
      if (distance < distMin && (warehouse(i).booleanProperty.value || init)) {
        distMin = distance
        nearestWarehouse = warehouse(i)
      }
    }
    nearestWarehouse
  }

  def addDeliveryToWarehouse(d: DeliveryStore, w: WareHouse, traceLine: Boolean = true): Unit = {
    w.nearestDeliveryStores = w.nearestDeliveryStores ++ Seq(d)
    if (traceLine) {
      addline(w, d)
    }
  }


  def updateNearestWarehouse(openWarehouses: Set[Int], warehousesToOpen: Set[Int], warehousesToClose: Set[Int]): Unit = {
    val task = new Runnable {
      override def run(): Unit = {
        //println(Thread.currentThread().getName)
        val warehousesCurrentlyOpen = openWarehouses
        val warehousesCurrentlyClose = warehouse.map(w => w._id).toSet.diff(openWarehouses)
        warehousesToClose.foreach(i => {
          val w = warehouse(i)
          w.booleanProperty.value = false
        })
        warehousesToOpen.foreach(i => {
          val w = warehouse(i)
          w.booleanProperty.value = true
        })
        warehousesCurrentlyOpen.foreach(i => {
          val w = warehouse(i)
          val deliveryStores = w.nearestDeliveryStores
          removeLines(w)
          w.nearestDeliveryStores = Seq()
          deliveryStores.foreach(d => addDeliveryToWarehouse(d, getNearestOpenWarehouse(d, distance)))
        })
        warehousesCurrentlyClose.foreach(i => {
          val w = warehouse(i)
          val deliveryStores = w.nearestDeliveryStores
          if (deliveryStores.nonEmpty) {
            removeLines(w)
            w.nearestDeliveryStores = Seq()
            deliveryStores.foreach(d => addDeliveryToWarehouse(d, getNearestOpenWarehouse(d, distance)))
          }
        })
      }
    }
    TPE.submit(task)
  }

  def update(openWarehouses: Set[Int], forcedToTraceLines: Boolean = false): Unit = {
    val task = new Runnable {
      override def run(): Unit = {
        val currentTime = watch.getTime
        warehouse.foreach(w => {
          w.booleanProperty.value = false
          if (currentTime - lastUpdate >= 1000 || forcedToTraceLines) {removeLines(w)}
          w.nearestDeliveryStores = Seq()
        })
        openWarehouses.foreach(i => {
          val w = warehouse(i)
          w.booleanProperty.value = true
        })
        if (currentTime - lastUpdate >= 1000 || forcedToTraceLines) {
          deliveryStores.foreach(d => addDeliveryToWarehouse(d,getNearestOpenWarehouse(d,distance)))
          lastUpdate = currentTime
        }
      }
    }
    TPE.submit(task)
  }
}
