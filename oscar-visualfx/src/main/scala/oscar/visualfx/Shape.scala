package oscar.visualfx

import scalafx.scene.layout.Region

abstract class Shape[T](position: T, id: Int, width: Double, height: Double) extends Region {
  val _id: Int = id
  val _width: Double = width
  val _height: Double = height
  var _pos: T = position

  def pos: T = _pos
  def pos_=(newPosition: T): Unit

  def setStyleSheet(path: String): Unit = {
    this.getStylesheets.add(getClass.getResource(path).toExternalForm)
  }
}
