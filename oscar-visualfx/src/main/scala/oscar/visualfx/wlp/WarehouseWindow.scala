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

package oscar.visualfx.wlp

/**
  * @author Rémi Barralis remi.barralis@yahoo.fr
  */

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

/**
  * Create a window to use for representing a warehouse location problem
  *
  * @param distanceCost the matrix of the distance costs
  * @param warehousePos the array of the warehouses' positions
  * @param deliveryPos the array of the deliveries' positions
  */
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

  stage.getScene.getStylesheets.add(getClass.getResource("../css/main.css").toExternalForm)
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

  /**
    * add all delivery positions to their nearest warehouses at initialization
    */
  deliveryStores.foreach(d => addDeliveryToWarehouse(d, getNearestOpenWarehouse(d, distanceCost, init = true), false))

  /**
    * Add a line between a delivery position and the nearest open warehouse
    *
    * @param startNode the node representing the warehouse
    * @param endNode the node representing the delivery position
    */
  def addLine(startNode: WareHouse, endNode: DeliveryStore): Unit = {
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

  /**
    * Remove all the lines owned by a warehouse
    *
    * @param w the warehouse where the lines have to be removed
    */
  def removeLines(w: WareHouse): Unit = {
    w.connectorLines.foreach(f => Platform.runLater(this.stackPane.children.removeAll(f)))
    w.connectorLines = Seq()
  }

  /**
    * Get the nearest open warehouse of a delivery position
    *
    * @param d the delivery position
    * @param distanceCost the matrix of the distance costs
    * @param init defines wether it's the initialisation or not
    * @return the nearest open warehouse of the given delivery position
    */
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

  /**
    * Add a delivery position a warehouse
    *
    * @param d the delivery position
    * @param w the warehouse
    * @param traceLine defines wether it has to add a line between the delivery position and the warehouse or not
    */
  def addDeliveryToWarehouse(d: DeliveryStore, w: WareHouse, traceLine: Boolean = true): Unit = {
    w.nearestDeliveryStores = w.nearestDeliveryStores ++ Seq(d)
    if (traceLine) {
      addLine(w, d)
    }
  }

  /**
    * Update the window
    *
    * @param openWarehouses the new array representing the open warehouses
    * @param forcedToTraceLines defines wether it has to add a line between the delivery position and the warehouse or not
    */
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

  /**
    * Start the internal watch
    */
  def startTimer: Unit = this.watch.start()

  /**
    * Stop the internal watch
    */
  def stopTimer: Unit = this.watch.stop()

  /**
    * Reset the window
    */
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
