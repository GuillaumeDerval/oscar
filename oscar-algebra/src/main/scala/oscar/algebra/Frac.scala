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
//class Frac(num: Expression[AnyType], denom: Expression[AnyType]) extends BinaryOp[AnyType] {
//  //require(!denom.isZero)
//  val a = num
//  val b = denom
//  val symb = "/"
//  val op = (a: Double, b: Double) => a / b
////  def derive(v: Var): Expression = {
////    val fprime = num.derive(v)
////    val gprime = denom.derive(v)
////    (fprime * denom - gprime * num) / (denom * denom)
////  }
//  override def isZero() = a.isZero()
//}
