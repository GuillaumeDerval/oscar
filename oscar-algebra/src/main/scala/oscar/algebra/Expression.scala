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

import scala.Numeric.Implicits._
import scala.annotation.tailrec

/**
  * Represents mathematical expression of any degree. [[Expression]]s can be added, multiplied and used to create equations (i.e. constraints).
  * @param num [[Numeric]] object for type [[V]]
  * @tparam T the degree of the [[Expression]]
  * @tparam V the type of values stored by the variables of the [[Expression]]. For now, mainly Double is used.
  */
abstract class Expression[+T <: ExpressionDegree, +V](implicit num: Numeric[V]) {

  /**
    * Returns the value of this [[Expression]] given the specified [[Model.Solution]]
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
    new NormalizedExpression[TP, VP](this.normalized.terms ++ that.normalized.terms)
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
    * Negate this [[Expression]]
    * @return -this
    */
  def unary_- = new NormalizedExpression(normalized.terms.map(_ * num.negate(num.one)))

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
  * [[Expression]] represented internally as a sum of [[Product]]s of [[Term]]s.
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
  def eval[VP >: V](env: Var[VP] => VP)(implicit numeric: Numeric[VP]): VP = {
    terms.map(_.eval(env)).sum
  }

  def uses[VP >: V](v: Var[VP]) = terms.exists(_ uses v)

  def ||:(s: String) = new NormalizedExpression(terms, s)

  override def toString = terms.mkString(" + ")

  def normalized[VP >: V](implicit numeric: Numeric[VP]) = this

  def isZero(): Boolean = false
}
  
