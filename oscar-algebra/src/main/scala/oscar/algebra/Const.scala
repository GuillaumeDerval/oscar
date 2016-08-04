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

class Const[+V: Numeric](val d: V) extends Term[Constant,V] {

  def eval[VR >: V](env: Var[VR] => VR)(implicit numeric: Numeric[VR]):VR = d
  def value = Some(d)


  override def toString = d.toString

  def apply[TP <: AnyType, VP >: V](that: Expression[TP,VP])(implicit op: (Expression[Constant,VP], Expression[TP,VP]) => ProdExpression[TP,VP], numeric: Numeric[VP]): ProdExpression[TP,VP] = {
    op(Const[VP](d), that)
  }

//  def *[TP <: AnyType, VP >: V](that: Expression[TP,VP])(implicit op: (Expression[Constant,VP], Expression[TP,VP]) => ProdExpression[TP,VP], numeric: Numeric[VP]): ProdExpression[TP,VP] = {
//    op(Const[VP](d), that)
//  }

  def toExpression[ VP >: V](implicit numeric: Numeric[VP]) = new Expression[Constant,VP](Stream(new Prod[Constant,VP](this,Seq())))

  //override def derive(v: Var): Expression = Zero

}

object Const {
  def apply[V](d: V)(implicit numeric: Numeric[V]): Const[V] = new Const(d)
  def unapply[V](c: Const[V]): Option[V] = Some(c.d)
}  
