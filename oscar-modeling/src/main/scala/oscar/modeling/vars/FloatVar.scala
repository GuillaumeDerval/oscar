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

package oscar.modeling.vars

import oscar.algo.vars.FloatVarLike
import oscar.modeling.algebra.Expression
import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.misc.VariableNotBoundException
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.vars.domainstorage.FloatDomainStorage

/**
 * Represents a variable with Integer domain
 * @param model_decl: the ModelDeclaration associated with this Var
 */
class FloatVar(model_decl: ModelDeclaration, id: Int, name: String) extends Var(model_decl, id, name) with FloatVarLike with FloatExpression {
  def getRepresentative: FloatVarImplem = model_decl.getCurrentModel.getRepresentative(this).asInstanceOf[FloatVarImplem]
  override def context = getRepresentative.context
  override def isBound: Boolean = getRepresentative.isBound
  override def max: Double = getRepresentative.max
  override def min: Double = getRepresentative.min
  override def hasValue(value: Double): Boolean = getRepresentative.hasValue(value)

  override def evaluate(): Double = if(isBound) max else throw new VariableNotBoundException()

  override def subexpressions(): Iterable[Expression] = Array[Expression]()
  override def mapSubexpressions(func: (Expression) => Expression): FloatExpression = this

  override def toString(): String = if(name.isEmpty) "FloatVar" else name

  /**
    * @return returns the set this variable represents, if it is bound
    */
  override def value(): Double = getRepresentative.value()

  /**
    * Returns true if continuous (not an integer variable)
    */
  override def continuous: Boolean = true

  /**
    * Returns true if the expression is linear
    */
  override def linear: Boolean = true

  override def reify()(implicit modelDeclaration: ModelDeclaration): FloatVar = this
}

object FloatVar {
  def apply(minValue: Double, maxValue: Double)(implicit model_decl: ModelDeclaration) = {
    new FloatVar(model_decl, model_decl.addNewRepresentative(new FloatDomainStorage(minValue, maxValue,"")), "")
  }
  def apply(minValue: Double, maxValue: Double, name: String)(implicit model_decl: ModelDeclaration) = {
    new FloatVar(model_decl, model_decl.addNewRepresentative(new FloatDomainStorage(minValue, maxValue,name)), name)
  }
}
