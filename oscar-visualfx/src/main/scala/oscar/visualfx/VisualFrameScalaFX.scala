package oscar.visualfx

import javafx.scene.SnapshotParameters
import javax.imageio.ImageIO
import oscar.visualfx.util.FileManager
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.embed.swing.SwingFXUtils._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, Button}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.scene.text._
import scalafx.scene.{Node, Scene}
import scalafx.stage.{Screen, Stage}

class VisualFrameScalaFX(_title: String) {

  val button = new Button("Save as PNG")

  val rectangle2D = Screen.primary.visualBounds

  val stage = new Stage {
    title = _title
    centerOnScreen()
  }

  stage.maxHeight = rectangle2D.getHeight
  stage.maxWidth = rectangle2D.getWidth

  val borderPane = new BorderPane {
    padding = Insets(10)
    top = new Text("Top")
    left = new Text("Left")
    right = new Text("Right")
    center = new Text("Center")
  }
  borderPane.getStyleClass.add("borderPane")
  stage.scene = new Scene(borderPane)
  stage.sizeToScene()
  stage.getScene.getStylesheets.add(getClass.getResource("../css/main.css").toExternalForm)
  this.borderPane.minHeightProperty().bind(stage.minHeightProperty())
  this.borderPane.minWidthProperty().bind(stage.minWidthProperty())

  stage.resizable = true

  val bottomHBox = new HBox(20) {
    alignment = Pos.BottomRight
    children = Seq(button)
  }

  borderPane.bottom = bottomHBox

  button.onAction = () => {
    val image = this.borderPane.snapshot(new SnapshotParameters, null)
    val fileManager = new FileManager
    val savedFile = fileManager.getFile(stage, false, "PNG")
    if (savedFile != null) ImageIO.write(fromFXImage(image, null), "png", savedFile)
    else new Alert(AlertType.Information) {
      contentText = "Not saved"
    }.showAndWait()
  }

  def getFrameNodes: Map[String, Node] = {
    val parent = this.borderPane
    val nodes = Map(
      "top" -> parent.top.asInstanceOf[scalafx.scene.Node],
      "bottom" -> parent.bottom.asInstanceOf[scalafx.scene.Node],
      "right" -> parent.right.asInstanceOf[scalafx.scene.Node],
      "left" -> parent.left.asInstanceOf[scalafx.scene.Node],
      "center" -> parent.center.asInstanceOf[scalafx.scene.Node])
    nodes
  }

  def setFrameNodes(position: String = "center", node: Node): Unit = {
    val parent = this.borderPane
    position match {
      case "center" => parent.center = node
      case "top" => parent.top = node
      case "bottom" => parent.bottom = node
      case "left" => parent.left = node
      case "right" => parent.right = node
    }
  }

  def showStage() {
    stage.show()
    println(Thread.currentThread().getName + " " + Thread.currentThread().getId)
  }

  stage.getScene.content.onChange({
    stage.sizeToScene()
    stage.centerOnScreen()
  })
}

object VisualFrameScalaFXExamples extends JFXApp {
  val visualApp = new VisualFrameScalaFX("")
  visualApp.showStage()
}
