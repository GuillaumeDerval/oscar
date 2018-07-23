package oscar.visualfx.trackopt

import scalafx.animation.TranslateTransition
import scalafx.application.JFXApp
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.scene.control.{Label, Tooltip}
import scalafx.scene.layout.Region
import scalafx.util.Duration

class Pos(x: Double, y: Double, stage: String, processDuration: Duration) {
  val _x: Double = x
  val _y: Double = y
  val _stage: String = stage
  var _processDuration: Duration = processDuration
  val accessibleProperty: BooleanProperty = new BooleanProperty {value = true}

  def duration: Duration = _processDuration

  def duration_=(d: Duration): Unit = {
    this._processDuration = d
  }
}

class Ladle(pos: Pos, info: String, id: Int, width: Int = 50, height: Double = 50) extends Region {

  val _id: Int = id
  var _pos: Pos = pos
  var _info: String = info
  var status: String = "Not working"
  val labelStatus: Label = new Label(pos._stage)
  val posProperty: ObjectProperty[Pos] = new ObjectProperty[Pos] {value = pos}

  Tooltip.install(this, new Tooltip(info))
  this.setMaxSize(width, height)
  this.setMinSize(width, height)
  this.autosize()
  this.translateY = pos._y
  this.translateX = pos._x
  this.toFront()

  def label: Label = labelStatus
  def information: String = _info
  def state: String = status
  def Pos: Pos = _pos

  def setStyleSheet(path: String): Unit = {
    this.getStylesheets.add(getClass.getResource(path).toExternalForm)
    this.getStyleClass.add("root")
  }

  def label_=(s: String): Unit = {
    this.labelStatus.text = s
    this.status = s
  }

  def information_=(s: String): Unit = {
    this._info = s
  }

  def Pos_=(newPos: Pos): TranslateTransition = {
    val oldX = this._pos._x
    val oldY = this._pos._y
    val transition = new TranslateTransition(new Duration(2000),this) {
      override def play(): Unit = {
        if (newPos.accessibleProperty.value) super.play()
        else super.pause()
      }
    }
    transition.fromX = oldX
    transition.toX = newPos._x
    transition.fromY = oldY
    transition.toY = newPos._y
    this._pos = newPos
    newPos.accessibleProperty.value = false
    transition.onFinished = event => {
      this.label = newPos._stage
      this.posProperty.value = newPos
    }
    transition
  }

}


object Examples extends JFXApp {
  val visualPlan = new VisualPlan("title")
  val pos = new Pos(0,0,"start", new Duration(0))
  val ladle = new Ladle(pos,"ladle info",1)
  ladle.toFront()
  visualPlan.stackPane.children.add(ladle)
  //ladle.xPos = 150
  //ladle.xPos = 100
  //ladle.yPos = -250
}