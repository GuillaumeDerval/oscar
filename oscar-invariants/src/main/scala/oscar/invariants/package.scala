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
package oscar

import scala.collection.mutable._

package object invariants {
  

  @inline def when[A](d: Occuring[A])(f: A => Boolean): Reaction[A] = {
    d.foreach(f)
  }
  
  implicit def bl2f[A,B](block: => B) = {a:A => block}
  implicit def rd2r[A](rd: ReactionDescription[A]) = rd.post()
  implicit def occuring2desc[A](occ: Occuring[A]) = occ ~> { _ => }
  @inline def perform[A](rd: ReactionDescription[A]): Reaction[A] = {
    for ( msg <- rd.occuring ){
      rd.f(msg)
      true
    }
  }
  @inline def whenever[A](e: Occuring[A])(f: A => Unit): Reaction[A] = {
    for ( msg <- e ){
      f(msg)
      true
    }
  }
  
  @inline def once[A](d: Occuring[A])(f: A => Unit) = {
    when(d){(x:A) => 
      f(x)
      false
    }
  }

  
  implicit def Var2Val[A](v: Var[A]) = {v()}
  implicit def array2ElementArray[A](at: scala.collection.immutable.IndexedSeq[Var[A]]) = {
    new ElementArray(at)
  }
  def sum(l: VarInt*) = {(v: VarInt) => new SumInvariant(v, l.toList)}
  def sumOnList(l: VarList[Int])    = {(v: VarInt) => new SumInvariantOnList(v, l)}
  def sumOnListOfVars(l: VarList[VarInt]) = {(v: VarInt) => new SumInvariantOnListOfVars(v, l)}

}
