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

package oscar.modeling.algebra.integer

import oscar.cp._
import oscar.cp.core.variables.CPIntVar

/**
  * An IntExpression that can be instantiated in a CP model
  */
trait CPInstantiableIntExpression extends IntExpression {
  /**
    * Post the expression, and return a CPIntVar corresponding to its value
    */
  def cpPostAndGetVar(cPSolver: CPSolver): CPIntVar

  /**
    * Post the expression, with 'v' being the value the expression should equal to
    * @param v The value the expression should equal to
    */
  def cpPostWithVar(cPSolver: CPSolver, v: CPIntVar): Unit
}
