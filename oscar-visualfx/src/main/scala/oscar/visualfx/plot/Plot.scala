package oscar.visualfx.plot

import java.lang
import java.util.concurrent.{LinkedBlockingDeque, ThreadPoolExecutor, TimeUnit}

import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.EventHandler
import javafx.scene.input.{MouseEvent, ScrollEvent}
import javafx.util.StringConverter
import org.apache.commons.lang.time.StopWatch
import oscar.visualfx.VisualFrameScalaFX
import scalafx.application.Platform
import scalafx.concurrent.{Service, Task}
import scalafx.geometry.Point2D
import scalafx.scene.chart.{LineChart, NumberAxis}
import scalafx.scene.chart.XYChart.{Data, Series}
import scalafx.scene.control.{CheckBox, Tooltip}

class Plot(xAxisIsTime: Boolean = false) extends VisualFrameScalaFX("Plot") {

  this.stage.getScene.getStylesheets.clear()
  this.stage.getScene.getStylesheets.add(getClass.getResource("../css/plot.css").toExternalForm)

  val TPE = new ThreadPoolExecutor(1,10,60,TimeUnit.SECONDS,new LinkedBlockingDeque[Runnable]())

  var iteration: Int = 0
  val watch = new StopWatch
  var minObjValue: Int = Int.MaxValue
  var lastData = Data[Number,Number](watch.getTime,minObjValue)
  var curveWasReachingBack: Boolean = false
  val scale_delta = 1.1
  var mouseOrgX: Double = _
  var mouseOrgY: Double = _

  val yAxis:NumberAxis = new NumberAxis(1,1,1) {autoRanging = true}
  val xAxis: NumberAxis = new NumberAxis(1,1,1) {autoRanging = true}
  val chart: LineChart[Number,Number] = new LineChart(xAxis,yAxis) {scaleShape = true}
  val serie = new Series[Number,Number]{name = "main series"}
  val minCheckbox = new CheckBox("Show min line")
  var minLineSeries: Seq[Series[Number,Number]] = Seq()

  chart.title = "Objective Function"
  chart.getData.add(serie)
  chart.animated = false
  chart.legendVisible = false
  minCheckbox.selectedProperty().addListener(new ChangeListener[lang.Boolean] {
    override def changed(observable: ObservableValue[_ <: lang.Boolean], oldValue: lang.Boolean, newValue: lang.Boolean): Unit = {
        if (newValue) {
          val addSeriesService = Service {Task {
            Plot.this.minLineSeries.foreach(s => Platform.runLater(Plot.this.chart.getData.add(s)))
          }}
          addSeriesService.start()
          addSeriesService.onSucceeded = event => {
            val mouseTransparentService = Service{Task{
              Plot.this.minLineSeries.foreach(s => s.getData.forEach(d => Platform.runLater(d.getNode.setMouseTransparent(true))))
            }}
            mouseTransparentService.start()
          }
        }
      else {
          val removeSeriesService = Service {Task{
            Platform.runLater(Plot.this.chart.getData.remove(1, Plot.this.chart.getData.size()))
          }}
          removeSeriesService.start()
        }
      }
    })

  xAxis.setTickLabelFormatter(new StringConverter[Number] {
    override def toString(`object`: Number): String = if (xAxisIsTime) {"%dms".format(`object`.intValue())} else {`object`.toString}

    override def fromString(string: String): Number = 0
  })

  yAxis.setTickLabelFormatter(new StringConverter[Number] {
    override def toString(`object`: Number): String = `object`.intValue().toString

    override def fromString(string: String): Number = 0
  })

  chart.setOnScroll(new EventHandler[ScrollEvent] {
    override def handle(event: ScrollEvent): Unit = {
      event.consume()
      if (event.getDeltaY == 0) {return}

      val mouseSceneCoordinates = new Point2D(event.getSceneX, event.getSceneY)
      val xInLocal = xAxis.sceneToLocal(mouseSceneCoordinates).getX
      val yInLocal = yAxis.sceneToLocal(mouseSceneCoordinates).getY
      val mX = xAxis.getValueForDisplay(xInLocal).doubleValue()
      val mY = yAxis.getValueForDisplay(yInLocal).doubleValue()

      val scaleFactor:Double = if (event.getDeltaY > 0 )  scale_delta else 1 / scale_delta
      xAxis.autoRanging = false
      yAxis.autoRanging = false
      val tickUnit = 25
      val newXUpperBound: Double = ((1+scaleFactor)*xAxis.getUpperBound - (1-scaleFactor)*xAxis.getLowerBound) / (2*scaleFactor)
      val newYUpperBound: Double = ((1+scaleFactor)*yAxis.getUpperBound - (1-scaleFactor)*yAxis.getLowerBound) / (2*scaleFactor)
      val newXLowerBound: Double = ((1+scaleFactor)*xAxis.getLowerBound - (1-scaleFactor)*xAxis.getUpperBound) / (2*scaleFactor)
      val newYLowerBound: Double = ((1+scaleFactor)*yAxis.getLowerBound - (1-scaleFactor)*yAxis.getUpperBound) / (2*scaleFactor)
      if (event.isControlDown) {
        yAxis.setUpperBound(newYUpperBound)
        yAxis.setLowerBound(newYLowerBound)
        yAxis.tickUnit = (newYUpperBound - newYLowerBound) / tickUnit
      }
      else {
        xAxis.setUpperBound(newXUpperBound)
        xAxis.setLowerBound(newXLowerBound)
        yAxis.setUpperBound(newYUpperBound)
        yAxis.setLowerBound(newYLowerBound)
        xAxis.tickUnit = (newXUpperBound - newXLowerBound) / tickUnit
        yAxis.tickUnit = (newYUpperBound - newYLowerBound) / tickUnit
      }
    }
  })

  chart.setOnMousePressed(new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      event.consume()
      if (event.getClickCount == 2) {
        xAxis.autoRanging = true
        yAxis.autoRanging = true
      }
      else {
        xAxis.autoRanging = false
        yAxis.autoRanging = false
      }
      val mouseSceneCoordinates = new Point2D(event.getSceneX, event.getSceneY)
      val xInLocal = xAxis.sceneToLocal(mouseSceneCoordinates).getX
      val yInLocal = yAxis.sceneToLocal(mouseSceneCoordinates).getY
      mouseOrgX = xAxis.getValueForDisplay(xInLocal).doubleValue()
      mouseOrgY = yAxis.getValueForDisplay(yInLocal).doubleValue()
    }
  })

  chart.setOnMouseDragged(new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      event.consume()
      val mouseSceneCoordinates = new Point2D(event.getSceneX, event.getSceneY)
      val xInLocal = xAxis.sceneToLocal(mouseSceneCoordinates).getX
      val yInLocal = yAxis.sceneToLocal(mouseSceneCoordinates).getY
      val mX = xAxis.getValueForDisplay(xInLocal).doubleValue()
      val mY = yAxis.getValueForDisplay(yInLocal).doubleValue()

      xAxis.setLowerBound(xAxis.getLowerBound - (mX - mouseOrgX))
      xAxis.setUpperBound(xAxis.getUpperBound - (mX - mouseOrgX))
      yAxis.setLowerBound(yAxis.getLowerBound - (mY - mouseOrgY))
      yAxis.setUpperBound(yAxis.getUpperBound - (mY - mouseOrgY))
    }
  })

  this.setFrameNodes(node = chart)
  this.bottomHBox.children.add(minCheckbox)
  this.showStage()

  def addPoint(objValue: Int, s: String = ""): Unit = {
    val timerNotStrated: Boolean = if (this.watch.getTime == 0) {
      this.watch.start()
      true
    } else {false}

    val value = objValue
    val minValue = minObjValue
    val currentTime = watch.getTime
    val it = iteration
    val lastValue = lastData.getYValue.intValue()
    val _lastData = Data[Number,Number](lastData.getXValue,lastData.getYValue.intValue() - 1)
    val _curveWasReachingBack = curveWasReachingBack
    val _minLineSeries = minLineSeries
    val curveIsReachingBack = !(value <= lastValue)
    val data = if (xAxisIsTime) {Data[Number,Number](currentTime,value)} else {Data[Number,Number](it,value)}
    val task = new Runnable {
      override def run(): Unit = {
        val tooltipText = if (xAxisIsTime) {"value = %d, time = %dms\nmove : %s".format(value,currentTime,s)} else {"value = %d, it = %d\nmove : %s".format(value,it,s)}
        val toolTip = if (timerNotStrated) {new Tooltip(tooltipText + "\n!Warning! : Timer was not started manually"){wrapText = true}} else {new Tooltip(tooltipText){wrapText = true}}
        Platform.runLater({
          Plot.this.serie.getData.add(data)
          Tooltip.install(data.getNode,toolTip)
        })
        if (curveIsReachingBack && lastValue <= minValue) {
          val minSerie = new Series[Number,Number]()
          val data1 = if (xAxisIsTime) {Data[Number,Number](currentTime,_lastData.getYValue)} else {Data[Number,Number](it,_lastData.getYValue)}
          minSerie.getData.addAll(_lastData,data1)
          Plot.this.minLineSeries = Plot.this.minLineSeries ++ Seq(minSerie)
        }
        else  if (value > minValue) {
          val lastMinDataValue = Plot.this.minLineSeries.last.getData.get(Plot.this.minLineSeries.last.getData.size()-1).getYValue
          val data1 = if (xAxisIsTime) {Data[Number,Number](currentTime,lastMinDataValue)} else {Data[Number,Number](it,lastMinDataValue)}
          Plot.this.minLineSeries.last.getData.add(data1)
        }
        if (value <= minValue) minObjValue = value
      }
    }
    lastData = data
    curveWasReachingBack = curveIsReachingBack
    TPE.submit(task)
    iteration += 1
  }

  def startTimer: Unit = this.watch.start()
  def stopTimer: Unit = this.watch.stop()

  def resetPlot: Unit = {
    TPE.purge()
    xAxis.autoRanging = true
    yAxis.autoRanging = true
    minCheckbox.selected = false
    iteration = 0
    minObjValue = Int.MaxValue
    stopTimer
    watch.reset()
    lastData = Data[Number,Number](watch.getTime,minObjValue)
    curveWasReachingBack = false
    minLineSeries = Seq()
    chart.getData.remove(1,chart.getData.size())
    chart.getData.get(0).getData.clear()
    println("reset")
  }
}
