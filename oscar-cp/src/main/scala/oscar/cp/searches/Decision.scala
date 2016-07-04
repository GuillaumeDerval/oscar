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

package oscar.cp.searches

import oscar.algo.search._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPOutcome.Failure
import oscar.algo.reversible.ReversibleContext


abstract class DomainDecision extends Decision {
  def opposite: DomainDecision
}

final class Remove(val variable: CPIntVar, val value: Int) extends DomainDecision {
  override def apply(): Unit = variable.store.remove(variable, value)
  override def toString: String = s"Remove(${variable.name}, $value)"
  override def opposite: DomainDecision = new Assign(variable, value)
}

final class Assign(val variable: CPIntVar, val value: Int) extends DomainDecision {
  override def apply(): Unit = variable.store.assign(variable, value)
  override def toString: String = s"Assign(${variable.name}, $value)"
  override def opposite: DomainDecision = new Remove(variable, value)
}

final class LowerEq(val variable: CPIntVar, val value: Int) extends DomainDecision {
  override def apply(): Unit = {
    val out = variable.updateMax(value)
    if (out == Failure) variable.store.fail()
    else variable.store.propagate()
  }
  override def toString: String = s"LowerEq(${variable.name}, $value)"
  override def opposite: DomainDecision = new GreaterEq(variable, value + 1)
}

final class GreaterEq(val variable: CPIntVar, val value: Int) extends DomainDecision {
  override def apply(): Unit = {
    val out = variable.updateMin(value)
    if (out == Failure) variable.store.fail()
    else variable.store.propagate()
  }
  override def toString: String = s"GreaterEq(${variable.name}, $value)"
  override def opposite: DomainDecision = new LowerEq(variable, value - 1)
}

object Decision {
  @inline final def remove(variable: CPIntVar, value: Int): Decision = new Remove(variable, value)
  @inline final def assign(variable: CPIntVar, value: Int): Decision = new Assign(variable, value)
  @inline final def lowerEq(variable: CPIntVar, value: Int): Decision = new LowerEq(variable, value)
  @inline final def greaterEq(variable: CPIntVar, value: Int): Decision = new GreaterEq(variable, value)
  @inline final def push(context: ReversibleContext): Decision = new Push(context)
  @inline final def pop(context: ReversibleContext): Decision = new Pop(context) 
  def apply(decision: => Unit): Alternative = () => decision
}
