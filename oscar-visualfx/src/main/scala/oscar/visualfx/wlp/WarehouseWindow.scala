package oscar.visualfx.wlp

import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

import org.apache.commons.lang.time.StopWatch
import oscar.visualfx.VisualFrameScalaFX
import scalafx.application.Platform
import scalafx.geometry.Point2D
import scalafx.scene.chart.XYChart.{Data, Series}
import scalafx.scene.chart.{NumberAxis, ScatterChart}
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Line

class WarehouseWindow(distanceCost: Array[Array[Int]], warehousePos: Array[(Int,Int)], deliveryPos: Array[(Int,Int)]) extends VisualFrameScalaFX("WLP") {

  val watch = new StopWatch
  var lastUpdate:Long = 1000
  val TPE = new ThreadPoolExecutor(1, 10, 60, TimeUnit.SECONDS, new LinkedBlockingQueue[Runnable])
  val distance: Array[Array[Int]] = distanceCost
  val offset = 50
  val Xmax: Int = List(warehousePos.map(_._1).max, deliveryPos.map(_._1).max).max + offset
  val Xmin: Int = List(warehousePos.map(_._1).min, deliveryPos.map(_._1).min).min - offset
  val Ymax: Int = List(warehousePos.map(_._2).max, deliveryPos.map(_._2).max).max + offset
  val Ymin: Int = List(warehousePos.map(_._2).min, deliveryPos.map(_._2).min).min - offset
  val xAxis = new NumberAxis(Xmin, Xmax, 1)
  val yAxis = new NumberAxis(Ymin, Ymax, 1)
  val scatterChart = new ScatterChart(xAxis, yAxis)
  val warehouse = Array.tabulate(warehousePos.length)(i => new WareHouse(this, warehousePos(i), i))
  val deliveryStores = Array.tabulate(deliveryPos.length)(i => new DeliveryStore(deliveryPos(i), i, 5))
  val warehouseSeries = new Series[Number, Number] {
    name = "Closed Warehouse"
  }
  val deliverySeries = new Series[Number, Number] {
    name = "Delivery Stores"
  }
  val stackPane = new Pane()
  stackPane.getStyleClass.add("pane")

  scatterChart.legendVisible = false
  scatterChart.animated = false
  scatterChart.setMinSize(900, 800)
  stackPane.children.addAll(scatterChart)
  scatterChart.prefHeightProperty().bind(stackPane.heightProperty())
  scatterChart.prefWidthProperty().bind(stackPane.widthProperty())
  this.setFrameNodes(node = stackPane)
  this.stage.sizeToScene()
  stage.resizable = false
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
    val startNodeBound = startNode.getBoundsInLocal
    val endNodeBound = endNode.getBoundsInLocal
    val startPoint = scatterChart.localToParent(startNode.localToParent(new Point2D(startNodeBound.getWidth/2,startNodeBound.getHeight/2)))
    val endPoint = scatterChart.localToParent(endNode.localToParent(new Point2D(endNodeBound.getWidth/2,endNodeBound.getHeight/2)))
    val line = new Line {
      fill = Color.Black
      stroke = Color.Black
      startX = startPoint.getX + scatterChart.getPadding.getLeft + borderPane.getPadding.getLeft + yAxis.getPadding.getLeft
      startY = startPoint.getY + scatterChart.getPadding.getTop + borderPane.getPadding.getTop + xAxis.getPadding.getLeft
      endX = endPoint.getX + scatterChart.getPadding.getLeft + borderPane.getPadding.getLeft + xAxis.getPadding.getLeft
      endY = endPoint.getY + scatterChart.getPadding.getTop + borderPane.getPadding.getTop + xAxis.getPadding.getLeft
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
    if (this.watch.getTime == 0) {this.watch.start()}
    val task = new Runnable {
      override def run(): Unit = {
        val currentTime = watch.getTime
        val refreshTime = 200
        warehouse.foreach(w => {
          w.booleanProperty.value = false
          if (currentTime - lastUpdate >= refreshTime || forcedToTraceLines) {removeLines(w)}
          w.nearestDeliveryStores = Seq()
        })
        openWarehouses.foreach(i => {
          val w = warehouse(i)
          w.booleanProperty.value = true
        })
        if (currentTime - lastUpdate >= refreshTime || forcedToTraceLines) {
          deliveryStores.foreach(d => addDeliveryToWarehouse(d,getNearestOpenWarehouse(d,distance)))
          lastUpdate = currentTime
        }
      }
    }
    TPE.submit(task)
  }

  def startTimer: Unit = this.watch.start()

  def stopTimer: Unit = this.watch.stop()

  def reset: Unit = {
    TPE.purge()
    lastUpdate = 1000
    stopTimer
    watch.reset()
    warehouse.foreach(w => {
      w.booleanProperty.value = false
      removeLines(w)
      w.nearestDeliveryStores.foreach(d => addDeliveryToWarehouse(d, getNearestOpenWarehouse(d, distanceCost, init = true), false))
    })
  }
}
