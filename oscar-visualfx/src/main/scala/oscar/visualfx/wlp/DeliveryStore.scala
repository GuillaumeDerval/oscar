package oscar.visualfx.wlp

import oscar.visualfx.shape.Shape
import scalafx.scene.control.Tooltip

class DeliveryStore(position:(Int,Int), id:Int, width:Double = 2) extends Shape[(Int,Int)](position,id,width,width) {

  this.setMinSize(width,width)
  this.setMaxSize(width,width)
  this.setStyleSheet("../css/DeliveryStore.css")
  this.getStyleClass.add("root")

  Tooltip.install(this, new Tooltip("delivery store : %d".format(id)))

  override def pos_=(newPosition: (Int, Int)): Unit = {
    this._pos = newPosition
  }

}
