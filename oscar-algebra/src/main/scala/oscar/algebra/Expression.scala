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

import scala.Numeric.Implicits._
import scala.annotation.tailrec


class Continuous(val doubleValue: Double){
  override def hashCode(): Int = doubleValue.hashCode()
  override def equals(other: Any) = {
    other match{
      case that: Continuous => that.doubleValue == doubleValue
      case _ => false
    }
  }
}
class Integral(val intValue: Int){
  override def hashCode(): Int = intValue.hashCode()
  override def equals(other: Any) = {
    other match{
      case that: Integral => that.intValue == intValue
      case _ => false
    }
  }
}

class AnyType

class Quadratic extends AnyType

class Linear extends Quadratic

class Constant extends Linear

object ProdExpression {
  def apply[T <: AnyType, V: Numeric](a: Expression[T, V], b: Expression[T, V]) = {
    val num = implicitly[Numeric[V]]
    new ProdExpression[T, V](
      for (va <- a.terms; vb <- b.terms) yield Prod(num.times(va.coef.d, vb.coef.d), va.vars ++ vb.vars)
    )
  }
}

class ProdExpression[+T <: AnyType, +V: Numeric](t: Stream[Prod[T, V]], n: String = "ND")(implicit num: Numeric[V]) extends Expression[T, V](t, n)(num)

object AnyType {


  implicit def times[V](implicit numeric: Numeric[V]): (Expression[AnyType, V], Expression[AnyType, V]) => ProdExpression[AnyType, V] = {
    ProdExpression(_, _)
  }

}

object Quadratic {


  implicit def times02[V](implicit numeric: Numeric[V]): (Expression[Constant, V], Expression[Quadratic, V]) => ProdExpression[Quadratic, V] = {
    ProdExpression(_, _)
  }

  implicit def times11[V](implicit numeric: Numeric[V]): (Expression[Linear, V], Expression[Linear, V]) => ProdExpression[Quadratic, V] = {
    ProdExpression(_, _)
  }


  implicit def times20[V](implicit numeric: Numeric[V]): (Expression[Quadratic, V], Expression[Constant, V]) => ProdExpression[Quadratic, V] = {
    ProdExpression(_, _)
  }

}

object Linear {

  implicit def times01[V](implicit numeric: Numeric[V]): (Expression[Constant, V], Expression[Linear, V]) => ProdExpression[Linear, V] = {
    ProdExpression(_, _)
  }


  implicit def times10[V](implicit numeric: Numeric[V]): (Expression[Linear, V], Expression[Constant, V]) => ProdExpression[Linear, V] = {
    ProdExpression(_, _)
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

  implicit def times00[V](implicit numeric: Numeric[V]): (Expression[Constant, V], Expression[Constant, V]) => ProdExpression[Constant, V] = {
    ProdExpression[Constant, V](_, _)
  }

  //
  //  implicit def eq[V]: (Expression[Constant], Expression[Constant]) => EQEquation[Constant] = {
  //    case (a, b) => new EQEquation[Constant](a - b)
  //  }
  //
  //  implicit def le[V]: (Expression[Constant], Expression[Constant]) => LQEquation[Constant] = {
  //    case (a, b) => new LQEquation[Constant](a - b)
  //  }
  //
  //  implicit def ge[V]: (Expression[Constant], Expression[Constant]) => GQEquation[Constant] = {
  //    case (a, b) => new GQEquation[Constant](a - b)
  //  }

}

abstract class AbstractExpression[+T <: AnyType, +V](implicit num: Numeric[V]) {
  def eval[VP >: V](env: Var[VP] => VP)(implicit numeric: Numeric[VP]): VP

  def value: Option[V]

  def toExpression[VP >: V](implicit numeric: Numeric[VP]): Expression[T, VP]

  def +[TP >: T <: AnyType, VP >: V](that: AbstractExpression[TP, VP])(implicit numeric: Numeric[VP]): Expression[TP, VP] = {
    new Expression[TP, VP](this.toExpression.terms ++ that.toExpression.terms)
  }

  def -[TP >: T <: AnyType, VP >: V](that: AbstractExpression[TP, VP])(implicit numeric: Numeric[VP]): Expression[TP, VP] = {

    this.toExpression.+(new Expression[TP, VP](that.toExpression.terms.map(_ * numeric.negate(numeric.one))))
  }

  def *[TP <: AnyType, TR <: AnyType, VP >: V](that: AbstractExpression[TP, VP])(implicit op: (Expression[T, VP], Expression[TP, VP]) => ProdExpression[TR, VP], numeric: Numeric[VP]): ProdExpression[TR, VP] = {
    op(this.toExpression, that.toExpression)
  }

  //  def *[TP >: T <: AnyType , VP>: V](v: VP)(implicit op: (Expression[Constant, VP], Expression[TP, VP]) => ProdExpression[TP, VP], numeric: Numeric[VP]): Expression[TP,VP] = {
  //    Const(v).*[TP,VP](this.toExpression)
  //  }

  def unary_- = new Expression(toExpression.terms.map(_ * num.negate(num.one)))

  //  def /[TP <: Quadratic, TR <: Quadratic](that.toExpression: AbstractExpression[TP])(implicit op: Function[(Expression[T],Expression[TP]), Expression[TR]]): Expression[TR] = {
  //    Sum(this.toExpression, that.toExpression)//op((this.toExpression, that.toExpression))
  //  }

  def <=[TR >: T <: AnyType, VP >: V](that: AbstractExpression[TR, VP])(implicit numeric: Numeric[VP]): LQEquation[TR, VP] = {
    new LQEquation[TR, VP](this.toExpression - that.toExpression)
  }

  def >=[TR >: T <: AnyType, VP >: V](that: AbstractExpression[TR, VP])(implicit numeric: Numeric[VP]): GQEquation[TR, VP] = {
    new GQEquation[TR, VP](this.toExpression - that.toExpression)
  }

  def ===[TR >: T <: AnyType, VP >: V](that: AbstractExpression[TR, VP])(implicit numeric: Numeric[VP]): EQEquation[TR, VP] = {
    new EQEquation[TR, VP](this.toExpression - that.toExpression)
  }
}

class Expression[+T <: AnyType, +V](val terms: Stream[Prod[T, V]], val name: String = "ND")(implicit num: Numeric[V]) extends AbstractExpression[T, V] {
  def eval[VP >: V](env: Var[VP] => VP)(implicit numeric: Numeric[VP]): VP = {
    terms.map(_.eval(env)).sum
  }

  def uses[VP >: V](v: Var[VP]) = terms.exists(_ uses v)

  def ||:(s: String) = new Expression(terms, s)

  def value: Option[V] = {
    @tailrec
    def loop(acc: V, stream: Stream[Option[V]]): Option[V] = {
      stream match {
        case Stream() => Some(acc)
        case Some(d) #:: tail => loop(acc * d, tail)
        case None #:: tail => None
      }
    }
    loop(num.one, terms.map(_.value))
  }

  override def toString = terms.mkString(" + ")

  def toExpression[VP >: V](implicit numeric: Numeric[VP]) = this

  

  //def derive(x: Var): Expression[T]

  def isZero(): Boolean = false
}
  
