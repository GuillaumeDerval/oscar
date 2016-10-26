package oscar.algebra

/**
 * Equation to be used in [[Model]]s. Represents `expr sense 0`
 *
 * @param expr [[Expression]] to be equated with `0`
 * @param sense <=, >= or ==
 * @param name string identifier for this equation
 * @tparam T degree of [[expr]]
 * @tparam V type of values of the variables in [[expr]]
 */
class Equation[+T <: ExpressionDegree,+V](val expr: NormalizedExpression[T,V], val sense: ConstraintSign, val name: String) {
  require(name.nonEmpty, s"Equation name should be non empty")
  override def toString: String = s"$expr $sense 0"

  /**
   * Returns the same [[Equation]] but with a different name.
   *
   * @param newName the new name of the [[Equation]]
   * @return a [[Equation]] with name `newName`
   */
  def |:(newName: String): Equation[T,V] = new Equation(expr, sense, newName)
}

/**
 * A set of [[Equation]]s that is not explicitly generated, but can be iterated. This enables to define large sets of
 * constraints in a [[Model]] without high memory usage.
 *
 * @tparam T degree of the [[Equation]]s in the [[EquationSystem]]
 * @tparam V values of the variables in the equations
 */
class EquationSystem[+T <: ExpressionDegree,+V](val equations: Iterable[Equation[T,V]], val name: String = "System"){

  /**
   * Returns the same [[EquationSystem]] but with a different name.
   *
   * @param newName the new name of the [[EquationSystem]]
   * @return a [[EquationSystem]] with name `newName`
   */
  def |:(newName: String): EquationSystem[T,V] = new EquationSystem(equations, newName)

  override def toString: String = {
    val res = new StringBuffer()
    res.append(name)
    res.append("\n--------------------\n")
    equations.foreach{eq =>
      res.append(eq)
      res.append("\n")
    }
    res.append("\n--------------------\n")

    res.toString
  }
}

/**
 * Sign of the [[Equation]]s.
 * @param symbol string representation of the sign
 * @param name name of the sign
 */
sealed abstract class ConstraintSign(val symbol: String, val name: String) {
  override def toString: String = symbol
}

case object LQ extends ConstraintSign("<=", "LQ")
case object EQ extends ConstraintSign("=", "EQ")
case object GQ extends ConstraintSign(">=", "GQ")

object ConstraintSign {
  val values: List[ConstraintSign] = List(LQ, EQ, GQ)
}

/**
 * Description of an [[Equation]].
 * @tparam T degree of the [[Equation]]
 * @tparam V type of values of [[Equation]]
 */
trait EquationDescription[+T <: ExpressionDegree,+V]{

  /**
   * Returns an [[Equation]] corresponding to this with name `name`
   * @param name name of the [[Equation]]
   */
  def |:(name: String): Equation[T,V]
}

class EQEquation[+T <: ExpressionDegree,+V](expr: NormalizedExpression[T,V]) extends EquationDescription[T,V]{
  def |:(s: String) = new Equation(expr, EQ,s)
}
class LQEquation[+T <: ExpressionDegree,+V](expr: NormalizedExpression[T,V]) extends EquationDescription[T,V]{
  def |:(s: String) = new Equation(expr, LQ,s)
}
class GQEquation[+T <: ExpressionDegree,+V](expr: NormalizedExpression[T,V])  extends EquationDescription[T,V]{
  def |:(s: String) = new Equation(expr, GQ,s)
}

