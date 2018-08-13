package oscar.examples.visualfx.wlp

import com.sun.javafx.stage.StageHelper
import oscar.cbls.core.computation.Store
import oscar.visualfx.plot.Plot
import oscar.visualfx.wlp.WarehouseWindow
import scalafx.application.JFXApp
import scalafx.concurrent.{Service, Task}
import scalafx.scene.control.Button


object WLPexample extends JFXApp {
  val startButton = new Button("Start")
  val stopButton = new Button("Stop")
  val restoreButton = new Button("Reset")
  var costForOpeningWarehouse1: Array[Int] = null
  var distanceCost: Array[Array[Int]] = null
  var warehousePositions: Array[(Int,Int)] = null
  var deliveryPositions: Array[(Int,Int)] = null
  var warehouseToWarehouseDistances: Array[Array[Int]] = null
  var wlp: WLP = null
  var window: WarehouseWindow = null
  val plot = new Plot(true)

  val initTask = Task ({
    wlp = new WLP
    val v = WarehouseLocationGenerator.problemWithPositions(wlp.W,wlp.D,0,1000,3)
    costForOpeningWarehouse1 = v._1
    distanceCost = v._2
    warehousePositions = v._3
    deliveryPositions = v._4
    warehouseToWarehouseDistances = v._5
  })

  val optimService = Service(Task({
    println("optim launched")
    wlp.optim(window,plot,distanceCost,costForOpeningWarehouse1,warehouseToWarehouseDistances)
  }))

  optimService.onSucceeded = succeededEvent => {println("optim finish")}

  startButton.onAction = startEvent => {optimService.start()}
  stopButton.onAction = stopEvent => {optimService.cancel()}
  restoreButton.onAction = resetEvent => {
    optimService.reset()
    plot.resetPlot
    window.reset
  }


  initTask.run()
  initTask.onSucceeded = event => {
    StageHelper.getStages.remove(1).hide()
    window = new WarehouseWindow(distanceCost,warehousePositions,deliveryPositions)
    window.bottomHBox.children.addAll(startButton,stopButton,restoreButton)
  }


}
