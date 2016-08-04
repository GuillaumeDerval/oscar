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

import oscar.algebra.{AnyType, Expression}
import oscar.dfo.modeling.DFOModel
import oscar.dfo.modeling.DFOFloatVar
import oscar.dfo.modeling.minimize
import oscar.dfo.modeling.onSolution

/**
 * @author pschaus@gmail.com
 */
object Himmelblau extends DFOModel with App {

  // declare two variables and their domain
  val x = DFOFloatVar("x1", -4, +4)
  val y = DFOFloatVar("x2", -4, +4)

  // Himmelblau function
  // 4 local minima: 
  //  f(-0.27,-0.92) = 181.61
  //  f(-3,2) = 0
  //  f(-2.8,-3.13) = 0
  //  f(-3.77,-3.28) = 0
  // f(-3.58,-1.84) = 0
  val objective: Expression[AnyType,Double] = (x * x + y - 11.0) * (x * x + y - 11.0) + ( y * y + x - 7.0) * ( y * y + x - 7.0)

  // callback to print evolution of objective during optimization
  onSolution {
    println(objective.value)
  }

  // start the effective optimization
  minimize(objective)

  println(""+x + " " + x.value)
  println(""+y + " " + y.value)
  println("objective:" + objective.value)

}
