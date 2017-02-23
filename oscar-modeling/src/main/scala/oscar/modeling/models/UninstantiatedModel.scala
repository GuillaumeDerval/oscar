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
import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.algebra.integer.IntExpression
import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.ModelVarStorage
import oscar.modeling.vars.{FloatVar, IntVar}
import oscar.modeling.vars.domainstorage.{FloatDomainStorage, IntDomainStorage}


/**
  * Uninstanciated model
  * @param declaration Model declarator
  * @param constraints The constraints applied to this model
  * @param intRepresentatives the variables id and the domains associated
  * @param optimisationMethod the eventual optimization method
  */
case class UninstantiatedModel(declaration: ModelDeclaration,
                               constraints: List[Constraint],
                               intRepresentatives: ModelVarStorage[IntVar, IntDomainStorage],
                               floatRepresentatives: ModelVarStorage[FloatVar, FloatDomainStorage],
                               optimisationMethod: OptimisationMethod) extends Model {
  override type IntVarImplementation = IntDomainStorage
  override type FloatVarImplementation = FloatDomainStorage

  /**
    * Add a new variable with a new domain
    *
    * @param domain: an IntVarImplementation
    * @return  the id to the newly created variable and a new Model containing the variable
    */
  def withNewVariable(domain: IntDomainStorage): (Int, UninstantiatedModel) = {
    val r = intRepresentatives.add(domain)
    (r._1, copy(intRepresentatives = r._2))
  }

  /**
    * Add a new variable with a new domain
    *
    * @param domain: an FloatVarImplementation
    * @return  the id to the newly created variable and a new Model containing the variable
    */
  def withNewVariable(domain: FloatDomainStorage): (Int, UninstantiatedModel) = {
    val r = floatRepresentatives.add(domain)
    (r._1, copy(floatRepresentatives = r._2))
  }

  /**
    * Post a new constraint
    *
    * @param constraint constraint to add
    * @return new model with the new constraint
    */
  def post(constraint: Constraint): UninstantiatedModel = copy(constraints = constraint :: constraints)

  /**
    * Post a new constraint
    *
    * @param constraint constraint to add
    * @return new model with the new constraint
    */
  def add(constraint: Constraint): UninstantiatedModel = post(constraint)

  /**
    * Post a new constraint
    *
    * @param constraint constraint to add
    * @return new model with the new constraint
    */
  def +(constraint: Constraint): UninstantiatedModel = add(constraint)

  /**
    * Minimize v
    *
    * @param v variable to minimize
    * @return copy of this model, but with minimisation of v
    */
  def minimize(v: Expression): UninstantiatedModel = v match {
    case intexpr: IntExpression => copy(optimisationMethod = Minimisation(intexpr))
    case floatexpr: FloatExpression => copy(optimisationMethod = MinimisationFloat(floatexpr))
  }

  /**
    * Maximize v
    *
    * @param v variable to maximize
    * @return copy of this model, but with maximisation of v
    */
  def maximize(v: Expression): UninstantiatedModel = v match {
    case intexpr: IntExpression => copy(optimisationMethod = Maximisation(intexpr))
    case floatexpr: FloatExpression => copy(optimisationMethod = MaximisationFloat(floatexpr))
  }

  /**
    * Maximize v
    *
    * @return copy of this model, but with optimisation disabled
    */
  def removeOptimisation(): UninstantiatedModel = copy(optimisationMethod = new NoOptimisation)
}