package oscar.visualfx.trackopt

import javafx.beans.value.{ChangeListener, ObservableValue}
import scalafx.application.JFXApp
import scalafx.scene.control.{Button, Label, Slider}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout._
import oscar.visualfx.VisualFrameScalaFX

class VisualPlan(title: String) {

  val visualFrame = new VisualFrameScalaFX(title)
  val stackPane = new StackPane()
  val image = new Image("oscar/visualfx/trackopt/TrackOpt_FENO_plant_layout_180502.png")
  val imageView = new ImageView(image)
  val speedSlider = new Slider(1, 100, 1)
  var speed: Double = speedSlider.getValue
  val sliderBox = new HBox(20)
  val sliderText = new Label("Speed :")
  val speedLabel = new Label("x%d".format(speed.toInt))
  val rightBox = new VBox(20)
  val startButton = new Button("Play")
  val pauseButton = new Button("Pause")
  val stopButton = new Button("Stop")
  val rightLabel = new Label("Ladle Status")


  rightBox.getStylesheets.add(getClass.getResource("RightBox.css").toExternalForm)
  rightBox.getStyleClass.add("root")
  rightLabel.getStyleClass.add("labelTitle")
  rightBox.children.add(rightLabel)
  sliderBox.children.addAll(sliderText,speedSlider,speedLabel,startButton,pauseButton,stopButton)
  sliderBox.getStylesheets.add(getClass.getResource("VisualPlan.css").toExternalForm)
  sliderBox.getStyleClass.add("hBox")
  sliderText.getStyleClass.add("textLeft")
  speedLabel.getStyleClass.add("textRight")
  stackPane.getStylesheets.add(getClass.getResource("VisualPlan.css").toExternalForm)
  stackPane.getStyleClass.add("root")
  speedSlider.getStylesheets.add(getClass.getResource("VisualPlan.css").toExternalForm)
  speedSlider.getStyleClass.add("slider")
  imageView.preserveRatio = true
  imageView.toBack()
  imageView.fitHeight = visualFrame.rectangle2D.getHeight / 1.2
  stackPane.children.add(imageView)
  stackPane.maxWidthProperty().bind(imageView.fitWidthProperty())
  stackPane.maxHeightProperty().bind(imageView.fitHeightProperty())
  visualFrame.setFrameNodes("center", stackPane)
  visualFrame.setFrameNodes("right", rightBox)
  visualFrame.setFrameNodes("bottom" , sliderBox)


  def addLadleState(ladle: Ladle): Unit = {
    val hBox = new HBox(5)
    hBox.getStyleClass.add("rightHBox")
    val label = new Label("Ladle " + ladle._id.toString + " : ")
    label.getStyleClass.add("rightLabel")
    ladle.label.getStylesheets.add(getClass.getResource("RightBox.css").toExternalForm)
    ladle.label.getStyleClass.add("rightState")
    hBox.children.addAll(label, ladle.label)
    this.rightBox.children.add(hBox)
  }

  this.rightBox.widthProperty().addListener(new ChangeListener[Number] {
    override def changed(observable: ObservableValue[_ <: Number], oldValue: Number, newValue: Number): Unit = {
      if (oldValue.doubleValue() < newValue.doubleValue()) {
        VisualPlan.this.visualFrame.sizeToScene()
        VisualPlan.this.visualFrame.centerOnScreen()
      }
    }
  })
}


object VisualPlanExamples extends JFXApp {
  val visualPlan = new VisualPlan("test")
  visualPlan.visualFrame.showStage
  /**val pos = new Pos(0,0,"start")
  val ladle = new Ladle(pos,"ladle info",1,1000)
  visualPlan.addLadleState(ladle)
  visualPlan.stackPane.children.add(ladle)
  ladle.toFront()**/
}