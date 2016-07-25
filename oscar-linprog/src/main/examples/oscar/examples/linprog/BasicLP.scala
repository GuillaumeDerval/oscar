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
import oscar.linprog.interface.lpsolve.LPSolveLib
import oscar.linprog.modeling._

object BasicLP extends MPModel(LPSolveLib) with App {

  val x0 = MPFloatVar("x0", 0, 40)
  val x1 = MPFloatVar("x1", 0, 1000)
  val x2 = MPFloatVar("x2", 0, 17)
  val x3 = MPFloatVar("x3", 2, 3)

  var cons = Array[LinearConstraint[_]]()

  maximize(x0 + x1*2.0 + x2*3.0 + x3)
  subjectTo(
    "cons1" ||: (x0 * -1 + x1     + x2 +  x3*10  <= 20),
    "cons2" ||: (     x0 - x1*3.0 + x2           <= 30),
    "cons3" ||: (          x1          -  x3*3.5 ===  0)
  )

  val endStatus = solver.solve

  println(s"End status = $endStatus")
  println(s"Solution quality = ${solver.solutionQuality}")
  println(s"Objective = ${solver.objectiveValue}")
  println("---------------------------------------------")
  println(s"x0: ${x0.value}")
  println(s"x1: ${x1.value}")
  println(s"x2: ${x2.value}")
  println(s"x3: ${x3.value}")
  println("---------------------------------------------")

  solver.release()
}
