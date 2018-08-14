package oscar.visualfx.plot

/**
  * @author Rémi Barralis remi.barralis@yahoo.fr
  **/
import java.lang
import java.util.concurrent.{LinkedBlockingDeque, ThreadPoolExecutor, TimeUnit}
import com.sun.javafx.stage.StageHelper
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.EventHandler
import javafx.scene.image.Image
import javafx.scene.input.{MouseButton, MouseDragEvent, MouseEvent, ScrollEvent}
import javafx.stage.Stage
import javafx.util.StringConverter
import org.apache.commons.lang.time.StopWatch
import oscar.visualfx.VisualFrameScalaFX
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.property.DoubleProperty
import scalafx.concurrent.{Service, Task}
import scalafx.geometry.{Insets, Point2D, Pos, Rectangle2D}
import scalafx.scene.chart.{LineChart, NumberAxis}
import scalafx.scene.chart.XYChart.{Data, Series}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, Button, CheckBox, Tooltip}
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.text.Text

/**
  * This class is use to plot the objective function
  *
  * You have to pass an instance of Plot to your neighborhood declaration and add the "addPoint" method in your "afterMove" statement
  *
  * @param xAxisIsTime defines wether the x-axis is time or iterations (it's iterations by default)
  */

class Plot(xAxisIsTime: Boolean = false) extends VisualFrameScalaFX("Objective Function") {
  /**
    * create the an executor to execute all the view changes in background tasks
    */
  val TPE = new ThreadPoolExecutor(1,10,60,TimeUnit.SECONDS,new LinkedBlockingDeque[Runnable]())

  var iteration: Int = 0
  val watch = new StopWatch
  var minObjValue: Int = Int.MaxValue
  var lastData = Data[Number,Number](watch.getTime,minObjValue)
  var curveWasReachingBack: Boolean = false
  val scale_delta = 1.1
  var mouseOrgX: Double = _
  var mouseOrgY: Double = _
  var mouseEndX: Double = _
  var mouseEndY: Double = _
  var mouseSceneCoordinates: Point2D = _
  val rectangleZoom: Rectangle = new Rectangle() {
    fill = Color.Transparent
    mouseTransparent = true
  }
  val yAxis: NumberAxis = new NumberAxis(1,1,1) {autoRanging = true}
  val xAxis: NumberAxis = new NumberAxis(1,1,1) {autoRanging = true}
  val chart: LineChart[Number,Number] = new LineChart(xAxis,yAxis) {scaleShape = true}
  val serie = new Series[Number,Number]{name = "main series"}
  val minCheckbox = new CheckBox("Show min line")
  var minLineSeries: Seq[Series[Number,Number]] = Seq()
  val pane = new Pane()
  val topText = new Text("Objective function plot")
  val helpButton = new Button()
  val helpDialog = new Alert(AlertType.Information)
  val helpStage: Stage = helpDialog.getDialogPane.getScene.getWindow.asInstanceOf[Stage]

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
  pane.autosize()
  pane.getStyleClass.add("pane")
  pane.children.addAll(chart,rectangleZoom)
  rectangleZoom.toBack()
  chart.prefHeightProperty().bind(pane.heightProperty())
  chart.prefWidthProperty().bind(pane.widthProperty())
  stage.setMinHeight(700)
  stage.setMinWidth(800)
  stage.resizable = false
  stage.getIcons.add(new Image(this.getClass.getResource("../img/app_icon.png").toString))
  topText.getStyleClass.add("title")
  topText.alignmentInParent = Pos.Center
  helpButton.getStyleClass.add("helpButton")
  Tooltip.install(helpButton,new Tooltip("Help"))
  helpStage.getIcons.add(new Image(getClass.getResource("../img/round_help_outline_black_18dp.png").toString))
  helpDialog.headerText = None
  helpDialog.title = "Help"
  helpDialog.contentText = "Zoom : mouse wheel\nVertical zoom : Ctrl + mouse wheel\nRectangle zoom : right click and drag\nReset zoom : double click\nMove the curve : left click and drag"
  helpButton.onAction = helpEvent => {
    helpDialog.showAndWait()
  }

  /**
    * creating and setting the event handlers to make the zoom possible
    */
  xAxis.setTickLabelFormatter(new StringConverter[Number] {
    override def toString(`object`: Number): String = if (xAxisIsTime) {"%dms".format(`object`.intValue())} else {"%.2f".format(`object`.doubleValue())}

    override def fromString(string: String): Number = 0
  })

  yAxis.setTickLabelFormatter(new StringConverter[Number] {
    override def toString(`object`: Number): String = "%.2f".format(`object`.doubleValue())

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
      mouseSceneCoordinates = new Point2D(event.getSceneX, event.getSceneY)
      val xInLocal = xAxis.sceneToLocal(mouseSceneCoordinates).getX
      val yInLocal = yAxis.sceneToLocal(mouseSceneCoordinates).getY
      mouseOrgX = xAxis.getValueForDisplay(xInLocal).doubleValue()
      mouseOrgY = yAxis.getValueForDisplay(yInLocal).doubleValue()
      if (event.getClickCount == 2 && event.getButton == MouseButton.PRIMARY) {
        xAxis.autoRanging = true
        yAxis.autoRanging = true
      }
      else {
        xAxis.autoRanging = false
        yAxis.autoRanging = false
      }
    }
  })

  chart.setOnDragDetected(new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      chart.startFullDrag()
    }
  })

  chart.setOnMouseDragged(new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      event.consume()
      if (event.getButton == MouseButton.PRIMARY) {
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
      else if (event.getButton == MouseButton.SECONDARY) {
        val mouseCoordinates = new Point2D(event.getSceneX, event.getSceneY)
        val mousePaneCoordinates = pane.sceneToLocal(mouseCoordinates)
        val xInLocal = xAxis.sceneToLocal(mouseCoordinates).getX
        val yInLocal = yAxis.sceneToLocal(mouseCoordinates).getY
        val maxY = List(mousePaneCoordinates.getY,pane.sceneToLocal(mouseSceneCoordinates).getY).max
        val minY = List(mousePaneCoordinates.getY,pane.sceneToLocal(mouseSceneCoordinates).getY).min
        val maxX = List(mousePaneCoordinates.getX,pane.sceneToLocal(mouseSceneCoordinates).getX).max
        val minX = List(mousePaneCoordinates.getX,pane.sceneToLocal(mouseSceneCoordinates).getX).min
        Platform.runLater({
          rectangleZoom.visible = true
          rectangleZoom.stroke = Color.Black
          rectangleZoom.toFront()
          rectangleZoom.translateX = minX //+ chart.getPadding.getLeft
          rectangleZoom.translateY = minY //+ chart.getPadding.getTop
          rectangleZoom.setHeight(maxY-minY)
          rectangleZoom.setWidth(maxX-minX)
        })
        mouseEndX = xAxis.getValueForDisplay(xInLocal).doubleValue()
        mouseEndY = yAxis.getValueForDisplay(yInLocal).doubleValue()
      }
    }
  })

  chart.setOnMouseDragReleased(new EventHandler[MouseDragEvent] {
    override def handle(event: MouseDragEvent): Unit = {
      event.consume()
      if (event.getButton == MouseButton.SECONDARY) {
        xAxis.setUpperBound(List(mouseOrgX,mouseEndX).max)
        xAxis.setLowerBound(List(mouseOrgX,mouseEndX).min)
        yAxis.setUpperBound(List(mouseOrgY,mouseEndY).max)
        yAxis.setLowerBound(List(mouseOrgY,mouseEndY).min)
        xAxis.tickUnit = (List(mouseOrgX,mouseEndX).max - List(mouseOrgX,mouseEndX).min) / 25
        yAxis.tickUnit = (List(mouseOrgY,mouseEndY).max - List(mouseOrgY,mouseEndY).min) / 25
        Platform.runLater({
          rectangleZoom.stroke = Color.Transparent
          rectangleZoom.toBack()
          rectangleZoom.visible = false
        })
      }
    }
  })

  /**
    * adding all graphical components to the window and making it popup
    */
  this.stage.getScene.getStylesheets.clear()
  this.stage.getScene.getStylesheets.add(getClass.getResource("../css/plot.css").toExternalForm)
  this.setFrameNodes("top", topText)
  this.setFrameNodes(node = pane)
  minCheckbox.alignmentInParent = Pos.Center
  this.bottomHBox.children.addAll(minCheckbox,helpButton)
  this.showStage()


  /**
    * Add a point to the Plot's line chart
    *
    * @param objValue the objective function's value
    * @param s the string representing the move done by the model
    */

  def addPoint(objValue: Int, s: String = ""): Unit = {
    val timerNotStarted: Boolean = if (this.watch.getTime == 0) {
      startTimer
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
        val toolTip = if (timerNotStarted) {new Tooltip(tooltipText + "\n!Warning! : Timer was not started manually"){wrapText = true}} else {new Tooltip(tooltipText){wrapText = true}}
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

  /**
    * Start the internal watch
    */
  def startTimer: Unit = this.watch.start()

  /**
    * Stop the internal watch
    */
  def stopTimer: Unit = this.watch.stop()

  /**
    * Add a vertical line at the current x position to emphasize a change in the search algorithm
    * @param s the string representing the new algorithm started
    */
  def addVLineMark(s: String): Unit = {
    val runnable = new Runnable {
      override def run(): Unit = {
        val x = if (xAxisIsTime) watch.getTime else iteration
        val xProperty = new DoubleProperty()
        val startYProperty = new DoubleProperty()
        val endYProperty = new DoubleProperty()
        Platform.runLater({
          val line = new Line
          xAxis.upperBound.addListener(new ChangeListener[Number] {
          override def changed(observable: ObservableValue[_ <: Number], oldValue: Number, newValue: Number): Unit = {
            val newXPos = xAxis.localToParent(new Point2D(xAxis.displayPosition(x),0)).getX + chart.getPadding.getLeft + pane.getPadding.getLeft + xAxis.getPadding.getLeft
            xProperty.value = newXPos
            if (x >= newValue.doubleValue()) {line.visible = false}
            else if (x <= xAxis.getLowerBound) {line.visible = false}
            else line.visible = true
          }
        })
          yAxis.upperBound.addListener(new ChangeListener[Number] {
          override def changed(observable: ObservableValue[_ <: Number], oldValue: Number, newValue: Number): Unit = {
            startYProperty.value = yAxis.localToParent(new Point2D(0,yAxis.displayPosition(newValue))).getY + chart.getPadding.getTop + pane.getPadding.getTop + yAxis.getPadding.getTop
          }
        })
          yAxis.lowerBound.addListener(new ChangeListener[Number] {
          override def changed(observable: ObservableValue[_ <: Number], oldValue: Number, newValue: Number): Unit = {
            endYProperty.value = yAxis.localToParent(new Point2D(0,yAxis.displayPosition(newValue))).getY + chart.getPadding.getTop + pane.getPadding.getTop + yAxis.getPadding.getTop
          }
        })
          line.stroke = Color.Black
          line.strokeWidth = 1
          line.startX.bind(xProperty)
          line.startY.bind(startYProperty)
          line.endX.bind(xProperty)
          line.endY.bind(endYProperty)
          val tooltip = new Tooltip(s)
          Tooltip.install(line,tooltip)
          pane.children.add(line)
          line.toFront()
        })
      }
    }
    TPE.submit(runnable)
  }

  /**
    * Reset the chart
    */
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

/**
  * Just an example
  */
object Examples extends JFXApp {

  var plot: Plot = _

  val task = Task {plot = new Plot()}

  task.run()
  task.onSucceeded = succeededEvent => {
    StageHelper.getStages.remove(1).hide()
    plot.addVLineMark("toto")
  }
}
