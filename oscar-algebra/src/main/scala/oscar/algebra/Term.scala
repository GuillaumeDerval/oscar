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

/**
 * Terms that cannot be represented as a [[NormalizedExpression]].
 * @tparam T the degree of the [[Expression]]
 * @tparam V the type of values stored by the variables of the [[Expression]]. For now, mainly Double is used.
 */
trait Term[+T <: ExpressionDegree,+V] extends Expression[T,V]{

}

object Const {
  def apply[V](d: V)(implicit numeric: Numeric[V]): Const[V] = new Const(d)
  def unapply[V](c: Const[V]): Option[V] = Some(c.d)
}

/**
 * Constant to be used in [[Expression]]
 * @param d the value of this constant
 * @tparam V the type of values stored by the variables of the [[Expression]]. For now, mainly Double is used.
 */
class Const[+V: Numeric](val d: V) extends Term[Constant,V] {

  def eval[VR >: V](env: Var[VR] => VR)(implicit numeric: Numeric[VR]): VR = d

  override def toString: String = d.toString

  def normalized[VP >: V](implicit numeric: Numeric[VP]): NormalizedExpression[Constant,VP] =
    new NormalizedExpression[Constant,VP](Stream(new Product[Constant,VP](this,Seq())))

  override def hashCode(): Int = d.hashCode
  override def equals(any: Any): Boolean = any match{
    case that: Const[V] => that.d.equals(d)
    case _ => false
  }
}

case object Zero extends Const(0)
case object One extends Const(1)
