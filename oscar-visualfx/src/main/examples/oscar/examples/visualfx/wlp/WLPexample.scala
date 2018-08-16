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

package oscar.examples.visualfx.wlp

import com.sun.javafx.stage.StageHelper
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
  // create the view for plotting the objective function
  val plot = new Plot(true)

  // create the task that will initialize the the optimisation problem
  val initTask = Task ({
    wlp = new WLP
    val v = WarehouseLocationGenerator.problemWithPositions(wlp.W,wlp.D,0,1000,3)
    costForOpeningWarehouse1 = v._1
    distanceCost = v._2
    warehousePositions = v._3
    deliveryPositions = v._4
    warehouseToWarehouseDistances = v._5
  })

  // create the service that will do the optimisation
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
    // create the window for showing the warehouses and the delivery positions
    window = new WarehouseWindow(distanceCost,warehousePositions,deliveryPositions)
    window.bottomHBox.children.addAll(startButton,stopButton,restoreButton)
  }


}
