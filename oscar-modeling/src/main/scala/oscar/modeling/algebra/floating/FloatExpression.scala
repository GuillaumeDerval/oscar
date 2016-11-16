package oscar.modeling.algebra.floating

import oscar.modeling.algebra.Expression
import oscar.modeling.algebra.bool._
import oscar.modeling.misc.{EmptyDomainException, VariableNotBoundException}
import oscar.modeling.models.ModelDeclaration

/**
  * Created by dervalguillaume on 16/11/16.
  */
trait FloatExpression extends Expression {
  /**
    * Evaluate this expression. All variables referenced have to be bound.
    * @throws VariableNotBoundException when a variable is not bound
    * @return the value of this expression
    */
  def evaluate(): Double

  /**
    * Return a *lower bound* for this expression
    */
  def min: Double

  /**
    * Return a *higher bound* for this expression
    */
  def max: Double

  /**
    * Returns true if continuous (not an integer variable)
    */
  def continuous: Boolean

  /**
    * Returns true if the expression is linear
    */
  def linear: Boolean

  /**
    * Returns an iterable that contains all sub-expressions of this expression
    */
  def subexpressions(): Iterable[FloatExpression]

  /**
    * Give a variable that is equal to this expression. May post appropriate constraints.
    * @param modelDeclaration the ModelDeclaration object in which new variable/constraint will be created
    * @throws EmptyDomainException when the new IntVar has an empty domain
    * @return an IntVar
    */
  /*def reify()(implicit modelDeclaration: ModelDeclaration): FloatVar = {
    val z = FloatVar(min, max)(modelDeclaration)
    modelDeclaration.post(EqFloat(this, z))
    z
  }*/

  def + (b: FloatExpression): FloatExpression = {
    (this, b) match {
      case (Sum(x), Sum(y)) => Sum(x ++ y)
      case (Sum(x), second) => Sum(x ++ Array(second))
      case (first,  Sum(y)) => Sum(y ++ Array(first))
      case (first,  second) => Sum(first, second)
    }
  }
  def - (b: FloatExpression): FloatExpression = Minus(this, b)
  def * (b: FloatExpression): FloatExpression = {
    (this, b) match {
      case (Prod(x), Prod(y)) => Prod(x ++ y)
      case (Prod(x), second ) => Prod(x ++ Array(second))
      case (first  , Prod(y)) => Prod(y ++ Array(first))
      case (first  , second ) => Prod(first, second)
    }
  }

  def / (b: FloatExpression): FloatExpression = Div(this, b)
  def / (b: Int): FloatExpression = DivConst(this, b)
  def / (b: Double): FloatExpression = DivConst(this, b)
  def % (b: Int): FloatExpression = Modulo(this, b)
  def ~** (b: FloatExpression): FloatExpression = Exponent(this, b)
  def ~^ (b: FloatExpression): FloatExpression = Exponent(this, b)
  def === (b: FloatExpression): BoolExpression = EqFloat(this, b)
  def === (b: Int): BoolExpression = EqFloat(this, Constant(b))
  def === (b: Double): BoolExpression = EqFloat(this, Constant(b))
  def !== (b: FloatExpression): BoolExpression = NotEqFloat(this, b)
  def !== (b: Int): BoolExpression = NotEqFloat(this, Constant(b))
  def !== (b: Double): BoolExpression = NotEqFloat(this, Constant(b))
  def >= (b: FloatExpression): BoolExpression = GrEqFloat(this, b)
  def > (b: FloatExpression): BoolExpression = GrFloat(this, b)
  def <= (b: FloatExpression): BoolExpression = LrEqFloat(this, b)
  def < (b: FloatExpression): BoolExpression = LrFloat(this, b)
  def unary_- : FloatExpression = UnaryMinus(this)
  def unary_+ : FloatExpression = this
  def unary_! : BoolExpression = this === 0

  /**
    * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
    * This function should return a value that is of the class as the object that was given to it.
    */
  override def mapSubexpressions(func: (Expression => Expression)): FloatExpression
}
