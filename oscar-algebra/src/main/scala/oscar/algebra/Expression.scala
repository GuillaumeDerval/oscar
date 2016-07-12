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

class AnyType
class Quadratic extends AnyType
class Linear extends Quadratic
class Constant extends Linear

object AnyType {
  implicit def sum: (Expression[AnyType], Expression[AnyType]) => SumExpression[AnyType] = { SumExpression(_,_) }
  implicit def diff: (Expression[AnyType], Expression[AnyType]) => Diff[AnyType] = { new Diff(_,_) }
  implicit def times: (Expression[AnyType], Expression[AnyType]) => Prod[AnyType] = { new Prod(_,_) }
  implicit def eq: (Expression[AnyType], Expression[AnyType]) => EQEquation[AnyType] = { case (a,b) => new EQEquation[AnyType](new Diff(a,b)) }
  implicit def le: (Expression[AnyType], Expression[AnyType]) => LQEquation[AnyType] = { case (a,b) => new LQEquation[AnyType](new Diff(a,b)) }
  implicit def ge: (Expression[AnyType], Expression[AnyType]) => GQEquation[AnyType] = { case (a,b) => new GQEquation[AnyType](new Diff(a,b)) }
}

object Quadratic {
  implicit def sum: (Expression[Quadratic], Expression[Quadratic]) => SumExpression[Quadratic] = { SumExpression(_,_) }
  implicit def diff: (Expression[Quadratic], Expression[Quadratic]) => Diff[Quadratic] = { new Diff(_,_) }
  implicit def times02: (Expression[Constant], Expression[Quadratic]) => Prod[Quadratic] = { new Prod(_,_) }
  implicit def times11: (Expression[Linear], Expression[Linear]) => Prod[Quadratic] = { new Prod(_,_) }
  implicit def times20: (Expression[Quadratic], Expression[Constant]) => Prod[Quadratic] = { new Prod(_,_) }
  implicit def eq: (Expression[Quadratic], Expression[Quadratic]) => EQEquation[Quadratic] = { case (a,b) => new EQEquation[Quadratic](new Diff(a,b)) }
  implicit def le: (Expression[Quadratic], Expression[Quadratic]) => LQEquation[Quadratic] = { case (a,b) => new LQEquation[Quadratic](new Diff(a,b)) }
  implicit def ge: (Expression[Quadratic], Expression[Quadratic]) => GQEquation[Quadratic] = { case (a,b) => new GQEquation[Quadratic](new Diff(a,b)) }
}
object Linear {
  implicit def sum: (Expression[Linear], Expression[Linear]) => SumExpression[Linear] = { SumExpression(_,_) }
  implicit def diff: (Expression[Linear], Expression[Linear]) => Diff[Linear] = { new Diff(_,_) }
  implicit def times01: (Expression[Constant], Expression[Linear]) => Prod[Linear] = { new Prod(_,_) }
  implicit def times10: (Expression[Linear], Expression[Constant]) => Prod[Linear] = { new Prod(_,_) }
  implicit def eq: (Expression[Linear], Expression[Linear]) => EQEquation[Linear] = { case (a,b) => new EQEquation[Linear](new Diff(a,b)) }
  implicit def le: (Expression[Linear], Expression[Linear]) => LQEquation[Linear] = { case (a,b) => new LQEquation[Linear](new Diff(a,b)) }
  implicit def ge: (Expression[Linear], Expression[Linear]) => GQEquation[Linear] = { case (a,b) => new GQEquation[Linear](new Diff(a,b)) }

}

object Constant {
  implicit def sum: (Expression[Constant], Expression[Constant]) => SumExpression[Constant] = { SumExpression(_,_) }
  implicit def diff: (Expression[Constant], Expression[Constant]) => Diff[Constant] = { new Diff(_,_) }
  implicit def times: (Expression[Constant], Expression[Constant]) => Prod[Constant] = { new Prod(_,_) }

  implicit def eq: (Expression[Constant], Expression[Constant]) => EQEquation[Constant] = { case (a,b) => new EQEquation[Constant](new Diff(a,b)) }
  implicit def le: (Expression[Constant], Expression[Constant]) => LQEquation[Constant] = { case (a,b) => new LQEquation[Constant](new Diff(a,b)) }
  implicit def ge: (Expression[Constant], Expression[Constant]) => GQEquation[Constant] = { case (a,b) => new GQEquation[Constant](new Diff(a,b)) }

}



abstract class Expression[+T <: AnyType] {

  def value: Option[Double]

  def eval(env: Var => Double): Double

  def +[TP >: T <: AnyType](that: Expression[TP]): SumExpression[TP] = {
    SumExpression[TP](this, that)
  }
  def -[TP <: AnyType, TR <: AnyType](that: Expression[TP])(implicit op: (Expression[T],Expression[TP])=> Diff[TR]): Diff[TR] = {
    op(this, that)
  }
  def *[TP <: AnyType, TR <: AnyType](that: Expression[TP])(implicit op: (Expression[T],Expression[TP])=> Prod[TR]): Prod[TR] = {
    op(this, that)
  }


  //  def /[TP <: Quadratic, TR <: Quadratic](that: Expression[TP])(implicit op: Function[(Expression[T],Expression[TP]), Expression[TR]]): Expression[TR] = {
//    Sum(this, that)//op((this, that))
//  }

  def <=[TP <: AnyType, TR <: AnyType](that: Expression[TP])(implicit op: (Expression[T],Expression[TP])=> LQEquation[TR]): LQEquation[TR] = {
    op(this, that)
  }
  def >=[TP <: AnyType, TR <: AnyType](that: Expression[TP])(implicit op: (Expression[T],Expression[TP])=> GQEquation[TR]): GQEquation[TR] = {
    op(this, that)
  }
  def ===[TP <: AnyType, TR <: AnyType](that: Expression[TP])(implicit op: (Expression[T],Expression[TP])=> EQEquation[TR]): EQEquation[TR] = {
    op(this, that)
  }

  //def derive(x: Var): Expression[T]

  def isZero(): Boolean = false
}
  
