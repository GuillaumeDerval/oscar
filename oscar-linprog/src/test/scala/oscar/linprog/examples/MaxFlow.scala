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

package oscar.linprog.examples

import oscar.algebra._
import oscar.linprog.interface.lpsolve.LPSolveLib
import oscar.linprog.modeling._

/**
 *  Note: example taken from glpk
 *  The Maximum Flow Problem in a network G = (V, E), where V is a set
 *  of nodes, E within V x V is a set of arcs, is to maximize the flow
 *  from one given node s (source) to another given node t (sink) subject
 *  to conservation of flow constraints at each node and flow capacities
 *  on each arc.
 */
object MaxFlow extends MPModel(LPSolveLib) with App {

  val Lines = 0 to 7
  val Columns = 0 to 8
  val nbcol = Columns.size
  val nbline = Lines.size
  val capa = Array(Array(0, 12, 0, 23, 0, 0, 0, 0, 0),
    Array(0, 0, 10, 9, 0, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 12, 0, 0, 18, 0),
    Array(0, 0, 0, 0, 26, 0, 0, 0, 0),
    Array(0, 11, 0, 0, 0, 25, 4, 0, 0),
    Array(0, 0, 0, 0, 0, 0, 7, 8, 0),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 15),
    Array(0, 0, 0, 0, 0, 63, 0, 0, 20))

  val x = Array.tabulate(nbline, nbcol)((l, c) => MPFloatVar("x" + (l, c), 0, capa(l)(c)))


  for (l <- 1 to nbline - 1)
    add(sum(Columns)(c => x(l)(c)) - sum(Lines)(c => x(c)(l)) =:= 0)

  maximize(sum(Lines)(l => x(l)(nbcol - 1)))

  solver.solve

  println("objective: " + solver.objectiveValue)
  for (l <- Lines) {
    for (c <- Columns)
      print(x(l)(c).value.get + "  ")
    println()
  }

  solver.release()
}
