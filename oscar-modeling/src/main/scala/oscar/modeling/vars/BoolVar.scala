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