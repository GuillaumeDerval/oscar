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

package oscar.visualfx

/**
  * @author Rémi Barralis remi.barralis@yahoo.fr
  */

import javafx.scene.SnapshotParameters
import javafx.scene.image.WritableImage
import javafx.scene.transform.Transform
import javax.imageio.ImageIO
import oscar.visualfx.util.FileManager
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.embed.swing.SwingFXUtils._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, Button}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.scene.{Node, Scene}
import scalafx.stage.{Screen, Stage}

/**
  * Class that create a window with a border pane layout
  *
  * @param _title the title of the window
  */
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
  }
  borderPane.getStyleClass.add("borderPane")
  stage.scene = new Scene(borderPane)
  stage.sizeToScene()
  this.borderPane.minHeightProperty().bind(stage.minHeightProperty())
  this.borderPane.minWidthProperty().bind(stage.minWidthProperty())

  stage.resizable = true

  val bottomHBox = new HBox(20) {
    alignment = Pos.CenterRight
    children = Seq(button)
  }

  borderPane.bottom = bottomHBox

  button.onAction = () => {
    val scale = 3
    val image = this.borderPane.getCenter.snapshot(new SnapshotParameters{setTransform(Transform.scale(scale,scale))}, new WritableImage(this.borderPane.getCenter.getBoundsInLocal.getWidth.toInt*scale,this.borderPane.getCenter.getBoundsInLocal.getHeight.toInt*scale))
    val fileManager = FileManager
    val savedFile = fileManager.getFile(stage, "PNG")
    if (savedFile != null) {
      ImageIO.write(fromFXImage(image,null),"png",savedFile)
    }
    else new Alert(AlertType.Information) {
      contentText = "Not saved"
      headerText = None
    }.showAndWait()
  }

  stage.getScene.content.onChange({
    stage.sizeToScene()
    stage.centerOnScreen()
  })

  /**
    * Getter of the border pane layout's components
    *
    * @return a Map mapping the string representation of the positions of the layout's components to them
    */
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

  /**
    * Setter of the border pane layout's components
    *
    * @param position the string representing the position of the layout's component
    * @param node the node of the graphical component to be set
    */
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

  /**
    * Make the window popup
    */
  def showStage() {
    stage.show()
    println(Thread.currentThread().getName + " " + Thread.currentThread().getId)
  }
}

/**
  * Just an example
  */
object VisualFrameScalaFXExamples extends JFXApp {
  val visualApp = new VisualFrameScalaFX("")
  visualApp.showStage()
}
