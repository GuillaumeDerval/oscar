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
package oscar.visual.shapes

import java.awt.Graphics2D
import java.awt.geom.Rectangle2D
import oscar.visual.VisualDrawing
import oscar.visual.VisualFrame
import java.awt.Font
import java.awt.Color

/**
 * @author Pierre Schaus, pschaus@gmail.com
 */
class VisualText(d: VisualDrawing, private var x: Double, private var y: Double, private var t: String, var centered: Boolean, s: Rectangle2D.Double) extends VisualShape(d) {
  
  type S = Rectangle2D.Double
  protected val shape = s
  
  def this(d: VisualDrawing, x: Int, y: Int, t: String, centered: Boolean = false) {
    this(d, x, y, t, centered, new Rectangle2D.Double(x, y, 1, 1))
  }

  var lines = text.trim.split("\n")
  def nLines = lines.length

  val fm = d.getFontMetrics(d.getFont)
  shape.setRect(x, y, lines.map(lineStr => fm.stringWidth(lineStr)).max, nLines * fm.getHeight)
  
  var font = new Font(Font.SANS_SERIF, Font.PLAIN, 13)
  var fontColor = Color.BLACK
  
  def setFont(font: Font) = {
    this.font = font
  }

  /**
   * Move the specified left corner
   * @param x the relative number of pixels to move along the x-axis
   * @param y the relative number of pixels to move along the y-axis
   */
  def move(x: Double, y: Double) {
    this.x = x
    this.y = y
    shape.setRect(x, y, shape.getWidth, shape.getHeight)
    drawing.repaint()
  }
  
  def text = t
  
  def text_=(t: String) {
    this.t = t
    lines = t.trim.split("\n")
    shape.setRect(x, y, lines.map(lineStr => fm.stringWidth(lineStr)).max, nLines * fm.getHeight)
    d.repaint()
  }

  override def draw(g: Graphics2D) {
    if (centered) {
      for (i <- 0 until nLines) {
        drawCenteredString(lines(i), x.toInt, y.toInt + i * fm.getHeight, g)
      }
    }
    else {
      for (i <- 0 until nLines) {
        g.drawString(lines(i), x.toInt, y.toInt + i * fm.getHeight)
      }
    }
    shape.setRect(x, y, fm.stringWidth(text), fm.getHeight)
  }

  def drawCenteredString(text: String, x: Int, y: Int, g: Graphics2D) {
    g.setFont(font)
    g.setColor(fontColor)
    val fm = g.getFontMetrics
    val w = fm.stringWidth(text)
    g.drawString(text, x - (w / 2), y)
  }
 
}

object VisualText extends App {

    val f = new VisualFrame("toto")
    val d = VisualDrawing(flipped=false)
    val inf = f.createFrame("Drawing")
    inf.add(d)
    f.pack()

    val arrow = VisualArrow(d, 50, 50, 100, 50, 5)
    val text = new VisualText(d, 50, 50, "hello\nworld")

    Thread.sleep(1000)

    arrow.dest = (100.0, 100.0)
    text.move(100, 100)

}
