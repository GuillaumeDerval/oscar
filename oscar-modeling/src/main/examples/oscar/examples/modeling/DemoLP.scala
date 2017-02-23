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

package oscar.examples.modeling

import oscar.modeling.solvers.SolverApp
import oscar.modeling.solvers.mip.MIPSolving
import oscar.modeling.vars.FloatVar

object DemoLP extends SolverApp[Unit] with MIPSolving {

  val x0 = FloatVar(0, 40, "x0")
  val x1 = FloatVar(0, 1000, "x1")
  val x2 = FloatVar(0, 17, "x2")
  val x3 = FloatVar(2, 3, "x3")

  val obj = x0 + 2 * x1 + 3 * x2 + x3
  maximize(obj)
  add(-1 * x0 +       x1   + x2 +  10 * x3 <=  20)
  add(     x0 - 3.0 * x1   + x2            <=  30)
  add(                x1        - 3.5 * x3 ===  0)

  onSolution {
    println("x0 = "+x0.value().toString)
    println("x1 = "+x1.value().toString)
    println("x2 = "+x2.value().toString)
    println("x3 = "+x3.value().toString)
    println("obj = "+obj.evaluate())
  }

  solve()
}