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
///**
// * (c * x)
// */
//class CstVar(val coeff: Const, val variable: Var) extends Expression[Linear] {
//
//  import scala.collection.immutable.Map
//  val cte = 0.0
//  val coef = if (coeff == 0) Map[Var, Double]() else Map(variable -> coeff.d)
//
//  override def toString = s"$coeff * $variable"
//
////  override def derive(v: Var): Expression = {
////    if (variable == v) coeff
////    else Zero
////  }
//
//}
//
//object CstVar {
//  def unapply(l: CstVar): Option[(Const, Var)] = Some(l.coeff, l.variable)
//}
