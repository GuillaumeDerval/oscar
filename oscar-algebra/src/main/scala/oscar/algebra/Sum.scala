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

object Sum{
  def apply[E <: Expression](a:E, b: E) = new Sum(Stream(a,b))
  def apply[E <: Expression](terms: Stream[E]) = new Sum(terms)
}

class  Sum[E <: Expression] private (terms: Stream[E]) extends Expression{
  def eval(env: Var => Double) = {
    terms.map(_.eval(env)).sum
  }

  def value = {
    @tailrec
    def loop(acc:Double, stream: Stream[Option[Double]]): Option[Double] = {
      stream match{
        case Stream() => Some(acc)
        case Some(d) #:: tail => loop(acc*d,tail)
        case None #:: tail => None
      }
    }
    loop(1.0,terms.map(_.value))
  }

  def derive(v: Var): Sum[Expression] = {
    Sum(terms.map(_.derive(v)))
  }
  override def isZero = terms.forall(_.isZero)
}
