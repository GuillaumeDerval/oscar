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

package oscar.modeling.algebra.bool

import oscar.cp.{CPBoolVar, CPSolver}

/**
  * A BoolExpression that can be instantiated in a CP model
  */
trait CPInstantiableBoolExpression extends BoolExpression {
  /**
    * Post the expression as a constraint (meaning the expression should be true)
    */
  def cpPostAsConstraint(cPSolver: CPSolver): Unit

  /**
    * Post the expression, and return a CPIntVar corresponding to its value
    */
  def cpPostAndGetVar(cPSolver: CPSolver): CPBoolVar

  /**
    * Post the expression, with 'v' being the value the expression should equal to
    * @param v The value the expression should equal to
    */
  def cpPostWithVar(cPSolver: CPSolver, v: CPBoolVar): Unit
}
