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

package oscar.cp.scheduling.visual

import oscar.visual._
import oscar.cp.scheduling._
import oscar.cp.core.variables.CPIntVar
import java.awt.Color
import oscar.visual.shapes.VisualText
import oscar.visual.shapes.VisualLine
import oscar.visual.shapes.VisualRectangle
import scala.Array.canBuildFrom

class VisualGanttChart(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], f: (Int) => Int, colors: (Int) => Color = i => Color.WHITE) extends VisualDrawing(false, false) {

  private val rectangles: Array[VisualRectangle] = Array.tabulate(starts.length)(a => {
    val rect = new VisualRectangle(this, 0, 0, 0, 0)
    rect.innerCol = colors(a)
    rect
  })

  private val max = starts.indices.map(i => f(i)).max

  private val text: VisualText = new VisualText(this, 50, 50, "")
  text.innerCol = Color.RED
  text.centered = true

  private val makespanLine: VisualLine = VisualLine(this, 0, 0, 0, 0)
  makespanLine.outerCol = Color.RED

  def update(xScale: Double, yScale: Double) {

    for (i <- starts.indices) {

      rectangles(i).width = durations(i).max * xScale
      rectangles(i).height = yScale

      rectangles(i).move(starts(i).min * xScale, f(i) * yScale)

      rectangles(i).innerCol = colors(i)
    }

    val makespan = ends.map(_.max).max

    makespanLine.orig = (makespan * xScale, 0)
    makespanLine.dest = (makespan * xScale, (max + 1) * yScale)

    text.text = makespan.toString
    text.move(makespan * xScale, (max + 2) * yScale)
    repaint()
  }
}
