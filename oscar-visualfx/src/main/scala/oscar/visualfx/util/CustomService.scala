package oscar.visualfx.util

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.concurrent.{Service, Task}
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.layout.HBox
import scalafx.stage.Stage


object ServiceExamples extends JFXApp {

  val service = Service(Task({
    println("the service runs task")
  }))

  val startButton = new Button("Start")
  val resetButton = new Button("Reset")

  startButton.onAction = startEvent => {service.start()}
  resetButton.onAction = resetEvent => {service.reset()}

  val hBox = new HBox(20) {children.addAll(startButton,resetButton)}

  this.stage = new PrimaryStage {
    title = "title"
    centerOnScreen()
  }
  this.stage.scene = new Scene(hBox)

}
