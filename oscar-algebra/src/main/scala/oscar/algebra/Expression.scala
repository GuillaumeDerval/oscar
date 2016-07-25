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

import scala.annotation.tailrec

class AnyType

class Quadratic extends AnyType

class Linear extends Quadratic

class Constant extends Linear

class ProdExpression[+T <: AnyType](a: Expression[T], b: Expression[T]) extends Expression[T](
  for (va <- a.terms; vb <- b.terms) yield new Prod(va.coef.d * vb.coef.d, va.vars ++ vb.vars)
)


object AnyType {


  implicit def times: (Expression[AnyType], Expression[AnyType]) => ProdExpression[AnyType] = {
    new ProdExpression(_, _)
  }

}

object Quadratic {


    implicit def times02: (Expression[Constant], Expression[Quadratic]) => ProdExpression[Quadratic] = {
      new ProdExpression(_, _)
    }

  implicit def times11: (Expression[Linear], Expression[Linear]) => ProdExpression[Quadratic] = {
    new ProdExpression(_, _)
  }


    implicit def times20: (Expression[Quadratic], Expression[Constant]) => ProdExpression[Quadratic] = {
      new ProdExpression(_, _)
    }

}

object Linear {

  implicit def times01: (Expression[Constant], Expression[Linear]) => ProdExpression[Linear] = {
    new ProdExpression(_, _)
  }



  implicit def times10: (Expression[Linear], Expression[Constant]) => ProdExpression[Linear] = {
    new ProdExpression(_, _)
  }
}

object Constant {


//  implicit def times0N[TP >: Linear <: AnyType]: (Expression[Constant], Expression[TP]) => ProdExpression[TP] = {
//    new ProdExpression(_, _)
//  }
//
//  implicit def timesN0[TP >: Linear <: AnyType]: (Expression[TP], Expression[Constant]) => ProdExpression[TP] = {
//    new ProdExpression(_, _)
//  }

  implicit def times00: (Expression[Constant], Expression[Constant]) => ProdExpression[Constant] = {
    new ProdExpression[Constant](_, _)
  }

  implicit def eq: (Expression[Constant], Expression[Constant]) => EQEquation[Constant] = {
    case (a, b) => new EQEquation[Constant](a - b)
  }

  implicit def le: (Expression[Constant], Expression[Constant]) => LQEquation[Constant] = {
    case (a, b) => new LQEquation[Constant](a - b)
  }

  implicit def ge: (Expression[Constant], Expression[Constant]) => GQEquation[Constant] = {
    case (a, b) => new GQEquation[Constant](a - b)
  }

}

trait AbstractExpression[+T <: AnyType] {
  //def eval(env: Var => Double): Double

  def value: Option[Double]

  def toExpression: Expression[T]
}

class Expression[+T <: AnyType](val terms: Stream[Prod[T]], val name: String = "ND") extends AbstractExpression[T] {
  def eval(env: Var => Double): Double = {
    terms.map(_.eval(env)).sum
  }

  def uses(v: Var) = terms.exists(_ uses v)

  def ||:(s: String) = new Expression(terms,s)

  def value: Option[Double] = {
    @tailrec
    def loop(acc: Double, stream: Stream[Option[Double]]): Option[Double] = {
      stream match {
        case Stream() => Some(acc)
        case Some(d) #:: tail => loop(acc * d, tail)
        case None #:: tail => None
      }
    }
    loop(1.0, terms.map(_.value))
  }

  override def toString = terms.mkString(" + ")

  def toExpression = this

  def +[TP >: T <: AnyType](that: Expression[TP]): Expression[TP] = {
    new Expression[TP](this.terms ++ that.terms)
  }

  def -[TP >: T <: AnyType](that: Expression[TP]): Expression[TP] = {

    this.+(new Expression[TP](that.terms.map(_ * -1)))
  }

  def *[TP <: AnyType, TR <: AnyType](that: Expression[TP])(implicit op: (Expression[T], Expression[TP]) => ProdExpression[TR]): ProdExpression[TR] = {
    op(this, that)
  }

  def unary_- = new Expression(terms.map(_ * (-1)))

  //  def /[TP <: Quadratic, TR <: Quadratic](that: Expression[TP])(implicit op: Function[(Expression[T],Expression[TP]), Expression[TR]]): Expression[TR] = {
  //    Sum(this, that)//op((this, that))
  //  }

  def <=[TR >: T <: AnyType](that: Expression[TR]): LQEquation[TR] = {
    new LQEquation[TR](this - that)
  }

  def >=[TR >: T <: AnyType](that: Expression[TR]): GQEquation[TR] = {
    new GQEquation[TR](this - that)
  }

  def ===[TR >: T <: AnyType](that: Expression[TR]): EQEquation[TR] = {
    new EQEquation[TR](this - that)
  }

  //def derive(x: Var): Expression[T]

  def isZero(): Boolean = false
}
  
