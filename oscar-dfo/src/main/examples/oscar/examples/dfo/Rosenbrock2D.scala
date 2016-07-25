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
package oscar.examples.dfo

import oscar.algebra.int2const
import oscar.dfo.modeling.DFOModel
import oscar.dfo.modeling.DFOFloatVar
import oscar.dfo.modeling.minimize
import oscar.dfo.modeling.onSolution

/**
 * @author pschaus@gmail.com
 */
object Rosenbrock2D extends DFOModel with App {



  // declare two variables and their domain
  val x = DFOFloatVar("x1", -10, +10)
  val y = DFOFloatVar("x2", -10, +10)

  // 2D Rosenbrock function
  val objective = (-x+1) * (-x+1) + 100 * (y - x * x) * (y - x * x)

  // callback to print evolution of objective during optimization
  onSolution {
    println(objective.value)
  }

  // start the effective optimization
  minimize(objective)

  println(x + " " + x.value)
  println(y + " " + y.value)
  println("objective:" + objective.value)

}
