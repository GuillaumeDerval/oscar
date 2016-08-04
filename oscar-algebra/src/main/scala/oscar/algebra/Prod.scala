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
trait Term[+T <: AnyType,+V] extends AbstractExpression[T,V]{


}

object Prod{
  def apply[T <: AnyType,V](vars: Seq[Term[T,V]])(implicit num: Numeric[V]) = new Prod(Const(num.one), vars)
  def apply[T <: AnyType,V](d: V, vars: Seq[Term[T,V]])(implicit num: Numeric[V]) = new Prod(Const(d), vars)

}

class Prod[+T <: AnyType,+V](val coef: Const[V], val vars: Seq[Term[T,V]])(implicit num: Numeric[V]) extends AbstractExpression[T,V]{
//  def derive[TR >: T <: AnyType](v: Var)(implicit op: Function[(Expression[T],Expression[T]), Prod[TR]]): Expression[TR] = {
//    a// * b.derive(v) + b * a.derive(v)
//  }

  def uses[VP>: V](v: Var[VP]) = vars.contains(v)

  override def toString = coef.toString + {if (vars.nonEmpty) vars.mkString("*","*","") else ""}

  def toExpression[VP>: V](implicit numeric: Numeric[VP]) = new Expression(Stream(this))

  def *[VP>: V](d: VP)(implicit num: Numeric[VP]) = new Prod(Const(num.times(coef.d, d)),vars)

  def eval[VP >: V](env: Var[VP] => VP)(implicit numeric: Numeric[VP]): VP = {
    var res:VP = coef.d
    for(v <- vars) res = numeric.times(res ,v.eval(env))
    res
  }
  def value: Option[V] = {
    @tailrec
    def loop(acc: V, stream: List[Term[T,V]]): Option[V] = {
      stream match {
        case Nil => Some(acc)
        case v :: tail => v.value match{
          case None => None
          case Some(d) => loop(acc*d, tail)
        }
      }
    }
    loop(coef.d, vars.toList)
  }

}
