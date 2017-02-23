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

object Product{
  def apply[T <: ExpressionDegree, V](vars: Seq[Var[V]])(implicit num: Numeric[V]): Product[T,V] = new Product[T,V](Const(num.one), vars)
  def apply[T <: ExpressionDegree, V](d: V, vars: Seq[Var[V]])(implicit num: Numeric[V]): Product[T,V] = new Product(Const(d), vars)
  def apply[V](d: V)(implicit num: Numeric[V]): Product[Constant,V] = new Product[Constant, V](Const(d), Seq())
  def apply[V](coef: Const[V])(implicit num: Numeric[V]): Product[Constant,V] = new Product[Constant, V](coef, Seq())

}

/**
 * Product of one [[Const]] and a set of [[Var]]s.
 * Represents {{{coef * vars.first * ... * vars.last}}}
 *
 * @param coef the coefficient of this [[Product]]
 * @param vars the variables of this [[Product]]
 * @param num [[Numeric]] object for type [[V]]
 * @tparam T the degree of the [[Expression]]
 * @tparam V the type of values stored by the variables of the [[Expression]]. For now, mainly Double is used.
 */
class Product[+T <: ExpressionDegree, +V](val coef: Const[V], val vars: Seq[Var[V]])(implicit num: Numeric[V]) extends Expression[T,V]{

  def uses[VP >: V](v: Var[VP]): Boolean = vars.contains(v)

  def normalized[VP >: V](implicit numeric: Numeric[VP]): NormalizedExpression[T,VP] = new NormalizedExpression(Iterable(this))

  def *[VP >: V](d: VP)(implicit num: Numeric[VP]): Product[T,VP] = new Product(Const(num.times(coef.d, d)), vars)
  def /[VP >: V](d: VP)(implicit num: Fractional[VP]): Product[T,VP] = new Product(Const(num.div(coef.d, d)), vars)

  def eval[VP >: V](env: Var[VP] => VP)(implicit numeric: Numeric[VP]): VP = {
    var res:VP = coef.d
    for(v <- vars) res = numeric.times(res ,v.eval(env))
    res
  }

  override def toString: String = coef.toString + {if (vars.nonEmpty) vars.mkString("*","*","") else ""}
}
