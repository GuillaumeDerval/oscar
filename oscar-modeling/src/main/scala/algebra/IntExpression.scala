package algebra

import misc.{EmptyDomainException, VariableNotBoundException}
import models.ModelDeclaration
import vars.IntVar

/**
 * An expression that represents an Integer
 */
trait IntExpression extends Serializable {
  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  def evaluate(): Int

  /**
   * Return a lower bound for this expression
   */
  def min: Int

  /**
   * Return a higher bound for this expression
   */
  def max: Int

  /**
   * Returns an iterable that contains a superset of the values this expression can have
   */
  def values(): Iterable[Int]

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  def subexpressions(): Iterable[IntExpression]

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  def mapSubexpressions(func: (IntExpression => IntExpression)): IntExpression

  /**
   * Give a variable that is equal to this expression. May post appropriate constraints.
   * @param modelDeclaration the ModelDeclaration object in which new variable/constraint will be created
   * @throws EmptyDomainException when the new IntVar has an empty domain
   * @return an IntVar
   */
  def reify()(implicit modelDeclaration: ModelDeclaration): IntVar = {
    val z = IntVar(min, max)(modelDeclaration)
    modelDeclaration.post(new Eq(this, z))
    z
  }

  def + (b: IntExpression): IntExpression = new BinarySum(this, b)
  def - (b: IntExpression): IntExpression = new Minus(this, b)
  def * (b: IntExpression): IntExpression = new Prod(this, b)
  def / (b: IntExpression): IntExpression = new Div(this, b)
  def % (b: Int): IntExpression = new Modulo(this, b)
  def ~** (b: IntExpression): IntExpression = new Exponent(this, b)
  def ~^ (b: IntExpression): IntExpression = new Exponent(this, b)
  def === (b: IntExpression): BoolExpression = new Eq(this, b)
  def === (b: Int): BoolExpression = new Eq(this, b)
  def !== (b: IntExpression): BoolExpression = new NotEq(this, b)
  def !== (b: Int): BoolExpression = new NotEq(this, b)
  def >= (b: IntExpression): BoolExpression = new GrEq(this, b)
  def > (b: IntExpression): BoolExpression = new Gr(this, b)
  def <= (b: IntExpression): BoolExpression = new LrEq(this, b)
  def < (b: IntExpression): BoolExpression = new Lr(this, b)
  def in (b: Set[Int]): BoolExpression = new InSet(this, b)
  def unary_- : IntExpression = new UnaryMinus(this)
  def unary_+ : IntExpression = this
  def unary_! : IntExpression = this !== 1

  def maxRegret(costs: Array[Int]): Int = {
    val valuess = values().toSeq
    var min1 = costs(valuess(0))
    var min2 = Int.MaxValue
    var i = valuess.length
    while (i > 1) {
      i -= 1
      val value = valuess(i)
      val cost = costs(value)
      if (cost <= min1) {
        min2 = min1
        min1 = cost
      } else if (cost < min2) min2 = cost
    }
    min2 - min1
  }
}

object IntExpression
{
  implicit def constant(v: Int): Constant = new Constant(v)
  implicit def array_element[A <% IntExpression](v: Array[A]): ArrayIntExpressionElementConstraintBuilder = new ArrayIntExpressionElementConstraintBuilder(v.asInstanceOf[Array[IntExpression]])
  implicit def array_element(v: Array[Int]): ArrayIntExpressionElementConstraintBuilderCst = new ArrayIntExpressionElementConstraintBuilderCst(v)
  implicit def array_element2D[A <% IntExpression](v: Array[Array[A]]): ArrayIntExpressionElementConstraintBuilder2D = new ArrayIntExpressionElementConstraintBuilder2D(v.asInstanceOf[Array[Array[IntExpression]]])
  implicit def array_element2D(v: Array[Array[Int]]): ArrayIntExpressionElementConstraintBuilderCst2D = new ArrayIntExpressionElementConstraintBuilderCst2D(v)

  class ArrayIntExpressionElementConstraintBuilder(val array: Array[IntExpression]) {
    def apply(id: IntExpression): IntExpression = new Element(array, id)
  }
  class ArrayIntExpressionElementConstraintBuilder2D(val array: Array[Array[IntExpression]]) {
    def apply(id1: IntExpression)(id2: IntExpression): IntExpression = new Element2D(array, id1, id2)
  }
  class ArrayIntExpressionElementConstraintBuilderCst(val array: Array[Int]) {
    def apply(id: IntExpression): IntExpression = new ElementCst(array, id)
  }
  class ArrayIntExpressionElementConstraintBuilderCst2D(val array: Array[Array[Int]]) {
    def apply(id1: IntExpression)(id2: IntExpression): IntExpression = new ElementCst2D(array, id1, id2)
  }

  implicit def array_intvar(v: Array[IntVar]): Array[IntExpression] = v.asInstanceOf[Array[IntExpression]]
}