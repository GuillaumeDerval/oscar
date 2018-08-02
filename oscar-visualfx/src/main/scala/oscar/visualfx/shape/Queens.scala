package oscar.visualfx.shape

import oscar.cbls.CBLSIntVar
import scalafx.application.Platform
import scalafx.concurrent.{Service, Task}
import scalafx.scene.layout.GridPane


class Queens(gridPane: GridPane, cBLSIntVar: CBLSIntVar, position: (Int,Int), id: Int, width: Double = 50, height: Double = 50) extends Shape[(Int, Int)](position, id, width, height) {

  var CBLSvar: CBLSIntVar = cBLSIntVar                                                                          

  setStyleSheet("trackopt/Queen.css")
  this.setMinSize(width,height)

  override def pos_=(newPosition: (Int, Int)): Unit = {
    val service = Service {
      Task {
        Platform.runLater({
          gridPane.add(this, newPosition._1, newPosition._2)
          this._pos = newPosition
        })
      }
    }
    service.start()
  }

  this.pos = position

}
