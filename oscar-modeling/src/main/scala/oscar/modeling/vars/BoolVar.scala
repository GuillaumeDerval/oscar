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

import oscar.algo.vars.BoolVarLike
import oscar.modeling.algebra.Expression
import oscar.modeling.algebra.bool.{BoolExpression, Not}
import oscar.modeling.constraints.{Constraint, ExpressionConstraint}
import oscar.modeling.misc.VariableNotBoundException
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.vars.domainstorage.IntDomainStorage

class BoolVar(model_decl: ModelDeclaration, id: Int, name: String) extends IntVar(model_decl, id, name) with BoolVarLike with BoolExpression
{
  /**
   * @return a constraint that imposes this variable is true
   */
  def constraintTrue(): Constraint = ExpressionConstraint(this)

  /**
   * @return a constraint that imposes this variable is false
   */
  def constraintFalse(): Constraint = ExpressionConstraint(Not(this))

  override def max: Int = getRepresentative.max
  override def min: Int = getRepresentative.min
  override def evaluate(): Int = if(isBound) max else throw new VariableNotBoundException()
  override def evaluateBool(): Boolean = evaluate() == 1

  override def subexpressions(): Iterable[BoolExpression] = Array[BoolExpression]()
  override def mapSubexpressions(func: (Expression) => Expression): BoolExpression = this
}

object BoolVar {
  def apply(containsFalse: Boolean, containsTrue: Boolean, name: String = "")(implicit model_decl: ModelDeclaration) = {
    new BoolVar(model_decl, model_decl.addNewRepresentative(IntDomainStorage(if (containsFalse) 0 else 1, if (containsFalse) 1 else 0, name)), name)
  }
  def apply()(implicit model_decl: ModelDeclaration) = new BoolVar(model_decl, model_decl.addNewRepresentative(IntDomainStorage(0,1)), "")
}