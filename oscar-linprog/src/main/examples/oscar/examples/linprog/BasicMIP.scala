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

package oscar.examples.linprog

import oscar.algebra._
import oscar.linprog.MPModel
import oscar.linprog.lpsolve.LPSolve

object BasicMIP extends MPModel(LPSolve) with App {

  val x0 = MPFloatVar("x0", 0.0, 40.0)
  val x1 = MPIntVar("x1", 0 to 1000)
  val x2 = MPIntVar("x2", 0 until 18)
  val x3 = MPFloatVar("x3", 2.0, 3.0)

  maximize(x0 + x1*2.0 + x2*3.0 + x3)
  subjectTo(
    "cons1" |: (-x0 +       x1   + x2 +   x3*10.0 <= 20.0),
    "cons2" |: (     x0 -  x1*3.0   + x2            <= 30.0),
    "cons3" |: (                x1        -  x3*3.5 ===  0.0)
  )

  val endStatus = interface.solve(this)

  println(s"End status = $endStatus")
  println("---------------------------------------------")
  println(s"x0: ${x0.value}")
  println(s"x1: ${x1.value}")
  println(s"x2: ${x2.value}")
  println(s"x3: ${x3.value}")
  println("---------------------------------------------")

}
