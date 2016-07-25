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

class Const(val d: Double) extends Term[Constant] {

  def eval(env: Var => Double) = d
  def value = Some(d)


  override def toString = d.toString

  def apply[TP <: AnyType](that: Expression[TP])(implicit op: (Expression[Constant], Expression[TP]) => ProdExpression[TP]): ProdExpression[TP] = {
    op(Const(d), that)
  }

  def *[TP <: AnyType](that: Expression[TP])(implicit op: (Expression[Constant], Expression[TP]) => ProdExpression[TP]): ProdExpression[TP] = {
    op(Const(d), that)
  }

  def toExpression = new Expression[Constant](Stream(new Prod(this,Seq())))

  //override def derive(v: Var): Expression = Zero

}

object Const {
  def apply(d: Double): Const = d match {
    case 0.0 => Zero
    case 1.0 => One
    case _ => new Const(d)
  }
  def unapply(c: Const): Option[Double] = Some(c.d)
}  
