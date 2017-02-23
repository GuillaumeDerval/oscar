/*******************************************************************************
<<<<<<< local
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
||||||| base
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

object BasicLP extends MPModel(LPSolve) with App {
=======
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
import oscar.linprog.lp_solve.LPSolve

object BasicLP extends MPModel(LPSolve) with App {
>>>>>>> other

<<<<<<< local
package oscar.examples.modeling
||||||| base
  val x0 = MPFloatVar("x0", 0, 40)
  val x1 = MPFloatVar("x1", 0, 1000)
  val x2 = MPFloatVar("x2", 0, 17)
  val x3 = MPFloatVar("x3", 2, 3)

  var cons = Array[LinearConstraint[_]]()
=======
  val x0 = VarNumerical("x0", 0, 40)
  val x1 = VarNumerical("x1", 0, 1000)
  val x2 = VarNumerical("x2", 0, 17)
  val x3 = VarNumerical("x3", 2, 3)

  var cons = Array[LinearConstraint]()
>>>>>>> other

<<<<<<< local
import oscar.modeling.solvers.SolverApp
import oscar.modeling.solvers.mip.MIPSolving
import oscar.modeling.vars.FloatVar
||||||| base
  maximize(x0 + x1*2.0 + x2*3.0 + x3)
  subjectTo(
    "cons1" |: (x0 * -1.0 + x1     + x2 +  x3*10.0  <= 20.0),
    "cons2" |: (     x0 - x1*3.0 + x2           <= 30.0),
    "cons3" |: (          x1          -  x3*3.5 ===  0.0)
  )

  val endStatus = interface.solve(this)
=======
  maximize(x0 + x1*2.0 + x2*3.0 + x3)
  subjectTo(
    "cons1" |: (x0 * -1.0 + x1     + x2 +  x3*10.0 <=  20.0),
    "cons2" |: (     x0 - x1*3.0 + x2              <=  30.0),
    "cons3" |: (          x1          -  x3*3.5    ===  0.0)
  )

  val endStatus = interface.solve(this)
>>>>>>> other

<<<<<<< local
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
}||||||| base
  println(s"End status = $endStatus")
  println("---------------------------------------------")
  println(s"x0: ${x0.value}")
  println(s"x1: ${x1.value}")
  println(s"x2: ${x2.value}")
  println(s"x3: ${x3.value}")
  println("---------------------------------------------")

}
=======
  println(s"End status = $endStatus")
  endStatus.onSolution { sol =>
    println("---------------------------------------------")
    println(s"x0: ${sol(x0)}")
    println(s"x1: ${sol(x1)}")
    println(s"x2: ${sol(x2)}")
    println(s"x3: ${sol(x3)}")
    println("---------------------------------------------")
  }
}
>>>>>>> other
