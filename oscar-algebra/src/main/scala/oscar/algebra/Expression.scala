/** *****************************************************************************
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
 * *****************************************************************************/
package oscar.algebra

/**
 * Represents mathematical expression of any degree. [[Expression]]s can be added, multiplied and used to create equations (i.e. constraints).
 * @param num [[Numeric]] object for type [[V]]
 * @tparam T the degree of the [[Expression]]
 * @tparam V the type of values stored by the variables of the [[Expression]]. For now, mainly Double is used.
 */
abstract class Expression[+T <: ExpressionDegree, +V](implicit num: Numeric[V]) {

  /**
   * Returns the value of this [[Expression]] given the specified [[Solution]]
   * @param env functions returing a value for all variables contained in this [[Expression]]
   * @param numeric [[Numeric]] object for [[VP]]
   * @tparam VP the type of values contained by the variables defined in env
   * @return the value of this [[Expression]] in function of env
   */
  def eval[VP >: V](env: Var[VP] => VP)(implicit numeric: Numeric[VP]): VP

  /**
   * Transforms ```this``` into normalized form
   * @param numeric [[Numeric]] object for VP
   * @tparam VP the type of values of the variables of this [[Expression]]
   * @return the [[NormalizedExpression]] representing this
   */
  def normalized[VP >: V](implicit numeric: Numeric[VP]): NormalizedExpression[T, VP]

  /**
   * Sum of two expressions
   * @param that the [[Expression]] to sum with this
   * @param numeric [[Numeric]] object for VP
   * @tparam TP the degree of that
   * @tparam VP the type of values stored by the variables in that
   * @return the sum of this and that, in normal form
   */
  def +[TP >: T <: ExpressionDegree, VP >: V](that: Expression[TP, VP])(implicit numeric: Numeric[VP]): NormalizedExpression[TP, VP] = {
    val thisTerms = this.normalized.terms
    val thatTerms = that.normalized.terms

    // Computes the new terms of the resulting expression by merging the common terms of the two expressions
    // TODO check groupBy for non-linear expressions. It assumes that the order of the vars is important (vars is a Seq), that is "x*y" is considered different from "y*x". It should be verified that it is indeed the case. See Pull Request #53.
    val terms = (thisTerms ++ thatTerms).groupBy(_.vars).map { case (vars, commonTerms) =>
      val sumOfTerms = commonTerms.map(_.coef.d).sum
      Product(sumOfTerms, vars)
    }

    new NormalizedExpression[TP,VP](terms)
  }

  /**
   * Difference of two expressions
   * @param that the [[Expression]] to substract from this
   * @param numeric [[Numeric]] object for VP
   * @tparam TP the degree of that
   * @tparam VP the type of values stored by the variables in that
   * @return the difference of this and that, in normal form
   */
  def -[TP >: T <: ExpressionDegree, VP >: V](that: Expression[TP, VP])(implicit numeric: Numeric[VP]): NormalizedExpression[TP, VP] = {
    this.normalized + (-that.normalized)
  }

  /**
   * Negate this [[Expression]]
   * @return -this
   */
  def unary_- = new NormalizedExpression(normalized.terms.map(_ * num.negate(num.one)))

  /**
   * Multiplication of two expressions
   * @param that the [[Expression]] to multiply with this
   * @param numeric [[Numeric]] object for VP
   * @tparam TP the degree of that
   * @tparam VP the type of values stored by the variables in that
   * @return the product of this and that, in normal form
   */
  def *[TP <: ExpressionDegree, TR <: ExpressionDegree, VP >: V](that: Expression[TP, VP])(implicit op: (NormalizedExpression[T, VP], NormalizedExpression[TP, VP]) => ProdExpression[TR, VP], numeric: Numeric[VP]): ProdExpression[TR, VP] = {
    op(this.normalized, that.normalized)
  }

  /**
   * Division of two expressions
   * @param that the [[Expression]] to divide with this
   * @param numeric [[Fractional]] object for VP
   * @tparam TP the degree of that
   * @tparam VP the type of values stored by the variables in that
   * @return the division of this and that, in normal form
   */
  def /[TP <: ExpressionDegree, TR <: ExpressionDegree, VP >: V](that: Expression[TP, VP])(implicit op: (NormalizedExpression[T, VP], NormalizedExpression[TP, VP]) => DivExpression[TR, VP], numeric: Fractional[VP]): DivExpression[TR, VP] = {
    op(this.normalized, that.normalized)
  }

  /**
   * Constraints that this is smaller of equal to that
   * @param that the other expression
   * @param numeric [[Numeric]] object for VP
   * @tparam TR the degree of that
   * @tparam VP the type of values of the variables in that
   * @return the constraint representing this <= that
   */
  def <=[TR >: T <: ExpressionDegree, VP >: V](that: Expression[TR, VP])(implicit numeric: Numeric[VP]): LQEquation[TR, VP] = {
    new LQEquation[TR, VP](this.normalized - that.normalized)
  }

  /**
   * Constraints that this is greater of equal to that
   * @param that the other expression
   * @param numeric [[Numeric]] object for VP
   * @tparam TR the degree of that
   * @tparam VP the type of values of the variables in that
   * @return the constraint representing this >= that
   */
  def >=[TR >: T <: ExpressionDegree, VP >: V](that: Expression[TR, VP])(implicit numeric: Numeric[VP]): GQEquation[TR, VP] = {
    new GQEquation[TR, VP](this.normalized - that.normalized)
  }

  /**
   * Constraints that this is equal to that
   * @param that the other expression
   * @param numeric [[Numeric]] object for VP
   * @tparam TR the degree of that
   * @tparam VP the type of values of the variables in that
   * @return the constraint representing this = that
   */
  def ===[TR >: T <: ExpressionDegree, VP >: V](that: Expression[TR, VP])(implicit numeric: Numeric[VP]): EQEquation[TR, VP] = {
    new EQEquation[TR, VP](this.normalized - that.normalized)
  }
}

/**
 * [[Expression]] represented internally as a sum of [[Product]]s.
 *
 * Each term should be unique, that is each [[Product]] in the sum should have a unique sequence of [[Var]].
 *
 * Please note that these expressions are not normalized enough yet to ensure that the [[equals]] method will work as
 * expected.
 *
 * @param terms the products in the sum
 * @param name identifying the [[Expression]]
 * @param num [[Numeric]] object for type [[V]]
 * @tparam T the degree of the [[Expression]]
 * @tparam V the type of values stored by the variables of the [[Expression]]. For now, mainly Double is used.
 */
class NormalizedExpression[+T <: ExpressionDegree, +V](val terms: Iterable[Product[T, V]], val name: String = "ND")(implicit num: Numeric[V]) extends Expression[T, V] {
  assert(terms.groupBy(_.vars).forall(_._2.size == 1), s"Similar terms where found in normalized expression $name. Each term should be unique.")

  /**
   * Returns the same [[NormalizedExpression]] but with a different name.
   *
   * @param newName the new name of the [[NormalizedExpression]]
   * @return a [[NormalizedExpression]] with name `newName`
   */
  def |:(newName: String) = new NormalizedExpression(terms, newName)

  def eval[VP >: V](env: Var[VP] => VP)(implicit numeric: Numeric[VP]): VP = {
    terms.map(_.eval(env)).sum
  }

  def uses[VP >: V](v: Var[VP]): Boolean = terms.exists(_ uses v)

  def normalized[VP >: V](implicit numeric: Numeric[VP]): NormalizedExpression[T, VP] = this

  def isZero: Boolean = false

  override def toString: String = terms.mkString(" + ")
}
  
