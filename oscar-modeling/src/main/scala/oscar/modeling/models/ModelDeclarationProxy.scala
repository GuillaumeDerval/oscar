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

package oscar.modeling.models

import oscar.modeling.algebra.Expression
import oscar.modeling.constraints.Constraint
import oscar.modeling.models.operators.ModelOperator

/**
  * Proxy to a ModelDeclaration
  */
trait ModelDeclarationProxy {
  val md: ModelDeclarationInterface
  def getCurrentModel = md.getCurrentModel
  def apply[RetVal](model: Model)(func: => RetVal): RetVal = md.apply[RetVal](model)(func)
  def post(constraint: Constraint): Unit = md.post(constraint)
  def post(constraints: Seq[Constraint]): Unit = constraints.foreach(md.post)
  def add(constraint: Constraint): Unit = md.add(constraint)
  def add(constraints: Seq[Constraint]): Unit = constraints.foreach(md.add)
  def minimize(v: Expression) = md.minimize(v)
  def maximize(v: Expression) = md.maximize(v)
  def removeOptimization() = md.removeOptimization()
  def apply[OutputType <: Model](operator: ModelOperator[OutputType]): Unit = md.apply[OutputType](operator)
  def fork[T](func: => T): T =  md.fork(func)
}