///*******************************************************************************
// * OscaR is free software: you can redistribute it and/or modify
// * it under the terms of the GNU Lesser General Public License as published by
// * the Free Software Foundation, either version 2.1 of the License, or
// * (at your option) any later version.
// *
// * OscaR is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU Lesser General Public License  for more details.
// *
// * You should have received a copy of the GNU Lesser General Public License along with OscaR.
// * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
// ******************************************************************************/
//package oscar.algebra
//
//class Diff[+T <: AnyType](val a: Expression[T], val b: Expression[T]) extends BinaryOp[T] {
//  val symb = "+"
//  val op = (a: Double, b: Double) => a - b
//  def derive[TP >: T <: AnyType](v: Var)(implicit op: Function[(Expression[TP],Expression[TP]), SumExpression[TP]]): Expression[TP] = {
//    a  //.derive(v).+(b.derive(v))
//  }
//  override def isZero() = a.isZero() && b.isZero()
//
//}
