package oscar.visualfx.routing

import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

import javafx.collections.ListChangeListener
import javafx.scene.chart.XYChart
import org.apache.commons.lang.time.StopWatch
import oscar.visualfx.VisualFrameScalaFX
import scalafx.application.{JFXApp, Platform}
import scalafx.scene.chart.LineChart.SortingPolicy
import scalafx.scene.chart.XYChart.{Data, Series}
import scalafx.scene.chart.{LineChart, NumberAxis, ScatterChart}
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color

class RoutingWindow(nodesPosition: Array[(Int,Int)], vehicles: Range) extends VisualFrameScalaFX("routing") {

  var vehiclesColor: Seq[Color] = Seq()
  val watch = new StopWatch
  var lastUpdate:Long = 0
  val TPE = new ThreadPoolExecutor(1, 10, 60, TimeUnit.SECONDS, new LinkedBlockingQueue[Runnable])
  val offset = 50
  val Xmax: Int = nodesPosition.map(_._1).max + offset
  val Xmin: Int = nodesPosition.map(_._1).min - offset
  val Ymax: Int = nodesPosition.map(_._2).max + offset
  val Ymin: Int = nodesPosition.map(_._2).min - offset
  val scatterChart = new LineChart(new NumberAxis(Xmin, Xmax, 1), new NumberAxis(Ymin, Ymax, 1))
  val data: Array[javafx.scene.chart.XYChart.Data[Number,Number]] = Array.tabulate(nodesPosition.length)(i => Data[Number,Number](nodesPosition(i)._1,nodesPosition(i)._2))
  val pane = new Pane()
  val series: Array[Series[Number,Number]] = Array.tabulate(vehicles.length)(i => new Series[Number,Number]{name = "vehicle : %d".format(i)})

  Color.getClass.getDeclaredFields.foreach(f => {
    f.setAccessible(true)
    if (f.getName.startsWith("Dark")) {vehiclesColor = vehiclesColor :+ f.get(Color).asInstanceOf[Color]}
  })

  scatterChart.legendVisible = false
  scatterChart.animated = false
  scatterChart.setAxisSortingPolicy(SortingPolicy.None)
  scatterChart.setMinSize(900, 800)
  pane.children.addAll(scatterChart)
  this.setFrameNodes(node = pane)
  this.stage.getScene.getStylesheets.add(getClass.getResource("../css/RoutingWindow.css").toExternalForm)

  series.foreach(s => {
    scatterChart.getData.add(s)
    s.getData.addListener(new ListChangeListener[XYChart.Data[Number, Number]] {
      override def onChanged(c: ListChangeListener.Change[_ <: XYChart.Data[Number, Number]]): Unit = {
        while (c.next()) {
          //println(c.getFrom)
          if (c.wasAdded() && !c.wasRemoved()) {
            if (c.getFrom == 0) {
              Platform.runLater(scatterChart.lookup(".chart-series-line").setStyle("-fx-stroke: #%s;".format(Integer.toHexString(vehiclesColor(s.getName.split(": ").last.toInt).hashCode()))))
            }
            c.getAddedSubList.forEach(d => Platform.runLater(d.getNode.lookup(".chart-line-symbol").setStyle("-fx-background-color: transparent;")))
          }
          else {
            Platform.runLater(c.getRemoved.forEach(d => {
              d.getNode.lookup(".chart-line-symbol").setStyle("-fx-background-color: transparent;")
            }))
          }
        }
      }
    })
  })

  this.stage.sizeToScene()
  this.showStage()

  def update(routesOfVehicles: Array[List[Int]],verbose: Boolean = false): Unit = {
    if (watch.getTime == 0) {watch.start()}
    if (watch.getTime - lastUpdate > 1000) {
      val runnable = new Runnable {
        override def run(): Unit = {
          for (i <- routesOfVehicles.indices) {
            val dataSet = scatterChart.getData.get(i).getData
            val routesOfVehicle = routesOfVehicles(i)
            if (verbose) println(routesOfVehicle.map(k => data(k)))
            for (l <- routesOfVehicle.indices) {
              val r = routesOfVehicle(l)
              val routeData = data(r)
              val currentData: Option[javafx.scene.chart.XYChart.Data[Number, Number]] = if (l < dataSet.size()) {
                Some(dataSet.get(l))
              } else None
              if (currentData.isEmpty) {
                for (j <- scatterChart.getData.toArray.indices) {
                  if (scatterChart.getData.get(j).getData.contains(routeData)) {
                    Platform.runLater({
                      scatterChart.getData.get(j).getData.remove(routeData)
                      if (verbose) println(routeData + " removed from " + scatterChart.getData.get(j).getName)
                    })
                  }
                }
                Thread.sleep(300)
                Platform.runLater({
                  if (verbose) println("data not found at index " + l)
                  dataSet.add(routeData)
                  if (verbose) println(routeData + " added to " + scatterChart.getData.get(i).getName)
                })
              }
              else if (!(currentData.get eq routeData)) {
                for (j <- scatterChart.getData.toArray.indices) {
                  if (scatterChart.getData.get(j).getData.contains(routeData)) {
                    Platform.runLater({
                      scatterChart.getData.get(j).getData.remove(routeData)
                      if (verbose) println(routeData + " removed from " + scatterChart.getData.get(j).getName)
                    })
                  }
                }
                Thread.sleep(300)
                Platform.runLater({
                  if (verbose) println(dataSet.get(l) + " replaced by " + routeData + " for " + scatterChart.getData.get(i).getName)
                  dataSet.set(l, routeData)
                })
              }
            }
          }
        }
      }
      TPE.submit(runnable)
      lastUpdate = watch.getTime
    }
  }

}

object Examples extends JFXApp {
  val vehicles = 0 until 10
  val window = new RoutingWindow(Array((0,0),(500,200),(56,10),(10,80),(88, 60),(0,0)),vehicles)
  println(window.scatterChart.getBoundsInLocal)
  println(window.pane.getBoundsInLocal)
}
