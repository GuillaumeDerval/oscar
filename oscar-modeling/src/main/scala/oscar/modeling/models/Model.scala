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

import oscar.modeling.misc.ModelVarStorage
import oscar.modeling.vars._

/**
 * Basic trait for all models
 */
trait Model extends Serializable {
  type IntVarImplementation <: IntVarImplem
  type FloatVarImplementation <: FloatVarImplem

  val declaration: ModelDeclaration
  val intRepresentatives: ModelVarStorage[IntVar, IntVarImplementation]
  val floatRepresentatives: ModelVarStorage[FloatVar, FloatVarImplementation]
  val optimisationMethod: OptimisationMethod

  /**
   * Get implementation of a Var
   * @param v variable to find
   * @return On an instantiated model, the model itself; on an uninstantiated one, a copy of it
   */
  def getRepresentative(v: IntVar): IntVarImplementation = intRepresentatives.get(v)
  def getRepresentative(v: FloatVar): FloatVarImplementation = floatRepresentatives.get(v)

  /**
   * Apply a function on this model
   * @param func
   */
  def apply[R](func: => R): R = declaration.apply(this)(func)
}
