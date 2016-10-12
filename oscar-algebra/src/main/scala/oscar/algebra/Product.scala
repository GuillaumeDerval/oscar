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
package oscar.algebra

import scala.annotation.tailrec
import Numeric.Implicits._



object Product{
  def apply[T <: ExpressionDegree,V](vars: Seq[Term[T,V]])(implicit num: Numeric[V]) = new Product(Const(num.one), vars)
  def apply[T <: ExpressionDegree,V](d: V, vars: Seq[Term[T,V]])(implicit num: Numeric[V]) = new Product(Const(d), vars)

}

/**
  * Product of one [[Const]] and a set of [[Term]]s. Represents
  * {{{coef * terms.first * ... * terms.last}}}
  * @param coef the coefficient of this [[Product]]
  * @param terms the terms of this [[Product]]
  * @param num [[Numeric]] object for type [[V]]
  * @tparam T the degree of the [[Expression]]
  * @tparam V the type of values stored by the variables of the [[Expression]]. For now, mainly Double is used.
  */
class Product[+T <: ExpressionDegree,+V](val coef: Const[V], val terms: Seq[Term[T,V]])(implicit num: Numeric[V]) extends Expression[T,V]{

  assert(terms.forall(! _.isInstanceOf[Const[_]]))
  def uses[VP>: V](v: Var[VP]) = terms.contains(v)

  override def toString = coef.toString + {if (terms.nonEmpty) terms.mkString("*","*","") else ""}

  def normalized[VP>: V](implicit numeric: Numeric[VP]) = new NormalizedExpression(Iterable(this))

  def *[VP>: V](d: VP)(implicit num: Numeric[VP]) = new Product(Const(num.times(coef.d, d)),terms)

  def eval[VP >: V](env: Var[VP] => VP)(implicit numeric: Numeric[VP]): VP = {
    var res:VP = coef.d
    for(v <- terms) res = numeric.times(res ,v.eval(env))
    res
  }

}
