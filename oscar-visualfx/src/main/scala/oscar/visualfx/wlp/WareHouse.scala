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

package oscar.visualfx.wlp

import java.lang

import javafx.beans.value.{ChangeListener, ObservableValue}
import oscar.visualfx.shape.Shape
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
  this.setStyleSheet("../css/WareHouse.css")
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
