package oscar.visualfx.wlp

import java.lang

import javafx.beans.value.{ChangeListener, ObservableValue}
import oscar.visualfx.Shape
import scalafx.application.Platform
import scalafx.beans.property.BooleanProperty
import scalafx.scene.control.Tooltip
import scalafx.scene.shape.Line

class WareHouse(window: WarehouseWindow, position: (Int,Int),id: Int, width: Double = 5, height: Double = 5) extends Shape[(Int,Int)](position,id,width,height) {

  val booleanProperty = new BooleanProperty {value = false}
  var nearestDeliveryStores: Seq[DeliveryStore] = Seq()
  var connectorLines: Seq[Line] = Seq()
  val _window: WarehouseWindow = window

  this.setMinSize(width,height)
  this.setMaxSize(width,height)
  this.setStyleSheet("css/WareHouse.css")
  this.getStyleClass.add("closedWarehouse")
  Tooltip.install(this, new Tooltip("warehouse : %d".format(id)))

  booleanProperty.addListener(new ChangeListener[lang.Boolean] {
    override def changed(observable: ObservableValue[_ <: lang.Boolean], oldValue: lang.Boolean, newValue: lang.Boolean): Unit = {
      //println(Thread.currentThread().getName + " " + Thread.currentThread().getId)
      Platform.runLater(WareHouse.this.getStyleClass.clear())
      if (newValue) {
        Platform.runLater({
          WareHouse.this.getStyleClass.add("openWarehouse")
          WareHouse.this.toFront()
        })
      }
      else {
        Platform.runLater({
          WareHouse.this.toBack()
          WareHouse.this.getStyleClass.add("closedWarehouse")
        })
      }
    }
  })

  override def pos_=(newPosition: (Int, Int)): Unit = {
    this._pos = newPosition
  }
}
