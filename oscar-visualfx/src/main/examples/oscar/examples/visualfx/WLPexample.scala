package oscar.examples.visualfx

import oscar.visualfx.wlp.WarehouseWindow
import scalafx.application.{JFXApp, Platform}
import scalafx.concurrent.{Service, Task}
import scalafx.scene.control.Button


object WLPexample extends JFXApp {
  val startButton = new Button("Start Optim")
  val stopButton = new Button("Stop")
  var costForOpeningWarehouse1: Array[Int] = null
  var distanceCost: Array[Array[Int]] = null
  var warehousePositions: Array[(Int,Int)] = null
  var deliveryPositions: Array[(Int,Int)] = null
  var warehouseToWarehouseDistances: Array[Array[Int]] = null
  var wlp: WLP = null
  var window: WarehouseWindow = null
  val initTask = Task {
    wlp = new WLP
    val v = WarehouseLocationGenerator.problemWithPositions(wlp.W,wlp.D,0,1000,3)
    costForOpeningWarehouse1 = v._1
    distanceCost = v._2
    warehousePositions = v._3
    deliveryPositions = v._4
    warehouseToWarehouseDistances = v._5
  }

  var optimTask = Task {
    window.watch.start()
    wlp.optim(window,distanceCost,costForOpeningWarehouse1,warehouseToWarehouseDistances)
  }
  val initService = Service(initTask)
  val optimService = Service(optimTask)

  initService.start()
  initService.onSucceeded = event => {
    window = new WarehouseWindow(distanceCost,warehousePositions,deliveryPositions)
    window.bottomHBox.children.addAll(startButton,stopButton)
    startButton.onAction = event => {optimService.start()}
    stopButton.onAction = event => {optimService.cancel}
  }

  optimService.onSucceeded = event => {
    println("optim finished")
  }


}
