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

import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.ModelVarStorage
import oscar.modeling.vars.domainstorage.{FloatDomainStorage, IntDomainStorage}
import oscar.modeling.vars.{FloatVar, IntVar}

trait LeafModel extends Model {
  /**
    * Post a new constraint
    * @param constraint constraint to add
    */
  def post(constraint: Constraint): Unit

  /**
    * Post a new constraint
    * @param constraint constraint to add
    */
  def add(constraint: Constraint): Unit = post(constraint)

  /**
    * Post a new constraint
    * @param constraint constraint to add
    */
  def += (constraint: Constraint): Unit = post(constraint)
}

/**
  * Abstract class for all Instantiated Models
  * @param parent: the model from which to inherit
  */
abstract class InstantiatedModel(val parent: UninstantiatedModel) extends LeafModel {
  override val declaration: ModelDeclaration = parent.declaration
  override val intRepresentatives: ModelVarStorage[IntVar, IntVarImplementation] = ModelVarStorage[IntVar, IntVarImplementation, IntDomainStorage](parent.intRepresentatives, instantiateIntDomainStorage)
  override val floatRepresentatives: ModelVarStorage[FloatVar, FloatVarImplementation] = ModelVarStorage[FloatVar, FloatVarImplementation, FloatDomainStorage](parent.floatRepresentatives, instantiateFloatDomainStorage)
  override val optimisationMethod: OptimisationMethod = parent.optimisationMethod

  // Post the constraints
  parent.constraints.foreach(add)

  // Post the max/minisation
  postObjective(parent.optimisationMethod)

  private def instantiateIntDomainStorage(v: IntDomainStorage): IntVarImplementation = instantiateIntVar(v.content, v.name)
  private def instantiateFloatDomainStorage(v: FloatDomainStorage): FloatVarImplementation = instantiateFloatVar(v.min, v.max, v.name)

  protected def instantiateIntVar(content: Iterable[Int], name: String): IntVarImplementation
  protected def instantiateFloatVar(min: Double, max: Double, name: String): FloatVarImplementation

  protected def postObjective(optimisationMethod: OptimisationMethod): Unit
}

/**
 * A InstantiatedModel that support forks (action that could be reverted immediately).
 * It is not mandatory for the fork{} action to be thread-safe.
 */
trait ForkableInstantiatedModel extends InstantiatedModel {
  /**
   * Fork (in the model tree). All the actions made on the model in a fork{} call will be reverted after the call.
   * It is not thread-safe.
   */
  def fork[T](func: => T): T
}