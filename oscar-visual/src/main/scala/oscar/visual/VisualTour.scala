package oscar.visual

import oscar.visual.shapes.{VisualArrow, VisualCircle, VisualLine}
import java.awt.Color

/**
 * 
 * 
 */
class VisualTour(nodesPos: Array[(Int, Int)], scale: Boolean, autoRepaint: Boolean) extends VisualDrawing(true, scale) {

  margin(20) // Prevents nodes from being positioned on the borders of the panel

  val nNodes = nodesPos.size

  // Edges have to be printed before nodes
  protected val edges: Array[VisualLine] = Array.tabulate(nNodes)(i => {
    val (x, y) = nodesPos(i)
    val edge = VisualArrow(this, x, y, x, y)

    edge.autoRepaint = false
    edge
  })

  protected val nodes: Array[VisualCircle] = Array.tabulate(nNodes)(i => {
    val (x, y) = nodesPos(i)
    val node = new VisualCircle(this, x, y, 1)
    node.autoRepaint = false
    node
  })

  def edgeDest(i: Int, dest: (Double, Double)): Unit = {
    edges(i).dest = dest
  }

  def edgeDest(i: Int, j: Int): Unit = {
    edgeDest(i, (nodes(j).getX, nodes(j).getY))
  }

  def edgeWidth(i: Int, width: Float): Unit = {
    edges(i).borderWidth = width
  }

  def edgeColor(i: Int, color: Color): Unit = {
    edges(i).outerCol = color
  }

  def nodeRadius(i: Int, radius: Double): Unit = {
    nodes(i).setRadius(radius)
  }

  def nodeColor(i: Int, color: Color): Unit = {
    nodes(i).innerCol = color
  }
}

object VisualTour {

  def apply(nodes: Array[(Int, Int)], scale: Boolean = true, autoRepaint: Boolean = false): VisualTour = {
    new VisualTour(nodes, scale, autoRepaint)
  }
}
