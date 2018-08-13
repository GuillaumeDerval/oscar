package oscar.examples.visualfx.routing

import com.sun.javafx.stage.StageHelper
import oscar.examples.cbls.routing.RoutingMatrixGenerator
import oscar.visualfx.plot.Plot
import oscar.visualfx.routing.RoutingWindow
import scalafx.application.JFXApp
import scalafx.concurrent.{Service, Task}
import scalafx.scene.control.Button

object RoutingExample extends JFXApp {

  val startButton = new Button("Start")
  val stopButton = new Button("Stop")
  val restoreButton = new Button("Reset")

  var vRPMaxDemo: VRPMaxDemo = _
  var symmetricNodeMatrix: Array[Array[Int]] = _
  var nodesPositions: Array[(Int,Int)] = _
  var plot: Plot = _
  var routingWindow: RoutingWindow = _

  val initTask = Task {
    plot = new Plot(xAxisIsTime = true)
    vRPMaxDemo = new VRPMaxDemo
    val routingGenerator = RoutingMatrixGenerator(vRPMaxDemo.n)
    symmetricNodeMatrix = routingGenerator._1
    nodesPositions = routingGenerator._2
    routingWindow = new RoutingWindow(nodesPositions,1 until vRPMaxDemo.v)
    routingWindow.bottomHBox.children.addAll(startButton,stopButton,restoreButton)
  }

  val optimService = Service(Task{
    vRPMaxDemo.optim(routingWindow,plot,symmetricNodeMatrix,nodesPositions)
  })

  startButton.onAction = startEvent => optimService.start()

  initTask.run()
  initTask.onSucceeded = event => {
    StageHelper.getStages.remove(2).hide()
    println("init done")
  }

}
