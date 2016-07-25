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

trait Term[+T <: AnyType] extends AbstractExpression[T]{


}

class Prod[+T <: AnyType](val coef: Const, val vars: Seq[Term[T]]) extends AbstractExpression[T]{
//  def derive[TR >: T <: AnyType](v: Var)(implicit op: Function[(Expression[T],Expression[T]), Prod[TR]]): Expression[TR] = {
//    a// * b.derive(v) + b * a.derive(v)
//  }

  def uses(v: Var) = vars.contains(v)

  override def toString = coef.toString + {if (vars.nonEmpty) vars.mkString("*","*","") else ""}

  def toExpression = new Expression(Stream(this))

  def *(d: Double) = new Prod(Const(coef.d * d),vars)

  def eval(env: Var => Double): Double = {
    var res = coef.d
    for(v <- vars) res = res * v.eval(env)
    res
  }
  def value: Option[Double] = {
    @tailrec
    def loop(acc: Double, stream: List[Term[T]]): Option[Double] = {
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
