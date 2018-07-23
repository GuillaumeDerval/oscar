package oscar.visualfx

import scalafx.application.JFXApp
import scalafx.scene.layout.GridPane

class Grid extends VisualFrameScalaFX("Gridpane") {

  val grid = new GridPane()
  grid.gridLinesVisible = true
  this.setMinHeight(rectangle2D.getHeight)

  this.setFrameNodes(node = grid)
}

object GridExamples extends JFXApp {
  val grid = new Grid
  val N = 100
  grid.showStage()
  val queenHeight: Double = (grid.stage.getMaxHeight - 200) / N
  //for (i <- 0 to N; j <- 0 to N) {new Queens(grid.grid,(i,j),i+j,queenHeight*4/5, height = queenHeight)}
  grid.sizeToScene()
  grid.centerOnScreen()

}