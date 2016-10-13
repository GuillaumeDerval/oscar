package oscar.modeling.algebra

import oscar.modeling.constraints.{Constraint, ExpressionConstraint}
import oscar.modeling.misc.{EmptyDomainException, VariableNotBoundException}
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.vars.BoolVar

/**
 * Represents a Boolean expression (an IntExpression that returns a boolean, 0 or 1)
 */
trait BoolExpression extends IntExpression {
  /**
   * Return a lower bound for this expression
   */
  def min: Int = 0

  /**
   * Return a higher bound for this expression
   */
  def max: Int = 1

  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  def evaluate(): Int = if(evaluateBool()) 1 else 0
  def evaluateBool(): Boolean

  /**
   * Give a variable that is equal to this expression. May post appropriate constraints.
   * @param modelDeclaration the ModelDeclaration object in which new variable/constraint will be created
   * @throws EmptyDomainException when the new IntVar has an empty domain
   * @return an IntVar
   */
  override def reify()(implicit modelDeclaration: ModelDeclaration): BoolVar = {
    val z = BoolVar(min == 0, max == 1)(modelDeclaration)
    modelDeclaration.post(this === z)
    z
  }

  /**
   * Get an iterator to all the values that this expression can take
   */
  override def values(): Iterable[Int] = Set(0, 1)

  def toConstraint: Constraint = ExpressionConstraint(this)
  def ^(b: BoolExpression): BoolExpression = Xor(this, b)
  def &(b: BoolExpression): BoolExpression = {
    (this, b) match {
      case (And(x), And(y)) => And(x ++ y)
      case (And(x), second) => And(x ++ Array(second))
      case (first , And(y)) => And(y ++ Array(first))
      case (first , second) => And(first, second)
    }
  }
  def |(b: BoolExpression): BoolExpression = {
    (this, b) match {
      case (Or(x), Or(y) ) => Or(x ++ y)
      case (Or(x), second) => Or(x ++ Array(second))
      case (first, Or(y) ) => Or(y ++ Array(first))
      case (first, second) => Or(first, second)
    }
  }
  def ==> (b: BoolExpression): BoolExpression = Implication(this, b)
}

object BoolExpression {
  /**
   * Convert a BoolExpression to an equivalent constraint
   */
  implicit def booltoConstraint(boolExpression: BoolExpression): Constraint = boolExpression.toConstraint
  implicit def toBoolExpression(intExpression: IntExpression): BoolExpression = intExpression !== 0
  implicit def intToConstraint(intExpression: IntExpression): Constraint = intExpression !== 0
}