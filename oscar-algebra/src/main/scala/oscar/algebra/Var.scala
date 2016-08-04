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


/** Abstract class for variables */
abstract class Var[+V: Numeric] extends Term[Linear,V] {
 require(name.nonEmpty)
  def name: String

  def id: Int

  def lowerBound: V

  def upperBound: V

  def eval[VP>:V](env: Var[VP] => VP)(implicit numeric: Numeric[VP])  = env(this)

  def toExpression[VP>: V](implicit numeric: Numeric[VP])  = new Expression(Stream( Prod(Seq(this))))

  override def toString = name

  //  override def derive(v: Var): Expression = {
  //    if (v equals this) One
  //    else Zero
  //  }

  override def equals(that: Any) = {
    that match {
      case other: Var[V] =>
        other.name equals this.name
      case _ => false
    }
  }

  override def hashCode: Int = name.hashCode
}


case class Var0[+V:Numeric](val name: String, val lowerBound: V, val upperBound: V)(implicit model: Model[_, _,V]) extends Var[V] {
  val id = model.addVariable(this)
  def value = None
}
