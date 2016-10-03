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
  * Variable to be used in [[Expression]]
  * @tparam V the type of values stored by the variables of the [[Expression]]. For now, mainly Double is used.
  */
abstract class Var[+V: Numeric] extends Term[Linear,V] {
 require(name.nonEmpty)

  /**
    * Unique string identifier for this variable
    */
  def name: String

  /**
    * Unique int id for this
    */
  def id: Int

  /**
    * Lower bound of this
    */
  def lowerBound: V

  /**
    * Upper bound of this
    */
  def upperBound: V

  /**
    * Value of the variable given a solution.
    * @param env functions returing a value for all variables contained in this [[Expression]]
    * @param numeric [[Numeric]] object for [[VP]]
    * @tparam VP the type of values contained by the variables defined in env
    * @return the value of this [[Expression]] in function of env
    */
  def eval[VP>:V](env: Var[VP] => VP)(implicit numeric: Numeric[VP])  = env(this)

  def normalized[VP>: V](implicit numeric: Numeric[VP])  = new NormalizedExpression(Stream( Product(Seq(this))))

  override def toString = name

  override def equals(that: Any) = {
    that match {
      case other: Var[V] =>
        other.id equals this.id
      case _ => false
    }
  }

  override def hashCode: Int = id.hashCode
}


case class Var0[+V:Numeric](val name: String, val lowerBound: V, val upperBound: V)(implicit model: Model[_, _,V]) extends Var[V] {
  val id = model.addVariable(this)
}
