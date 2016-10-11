package vars

import algebra.{BoolExpression, IntExpression, Not}
import constraints.{Constraint, ExpressionConstraint}
import misc.VariableNotBoundException
import models.ModelDeclaration
import vars.domainstorage.int.IntDomainStorage

class BoolVar(model_decl: ModelDeclaration, id: Int) extends IntVar(model_decl, id) with BoolVarLike with BoolExpression
{
  /**
   * @return a constraint that imposes this variable is true
   */
  def constraintTrue(): Constraint = new ExpressionConstraint(this)

  /**
   * @return a constraint that imposes this variable is false
   */
  def constraintFalse(): Constraint = new ExpressionConstraint(new Not(this))

  override def max: Int = getRepresentative.max
  override def min: Int = getRepresentative.min
  override def evaluate(): Int = if(isBound) max else throw new VariableNotBoundException()
  override def evaluateBool(): Boolean = evaluate() == 1

  override def subexpressions(): Iterable[IntExpression] = Array[IntExpression]()
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = this
}

object BoolVar {
  def apply(containsFalse: Boolean, containsTrue: Boolean, name: Option[String] = None)(implicit model_decl: ModelDeclaration) = {
    new BoolVar(model_decl, model_decl.addNewRepresentative(IntDomainStorage(if (containsFalse) 0 else 1, if (containsFalse) 1 else 0, name)))
  }
  def apply()(implicit model_decl: ModelDeclaration) = new BoolVar(model_decl, model_decl.addNewRepresentative(IntDomainStorage(0,1)))
}