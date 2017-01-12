package oscar.cbls.lib.invariant.seq

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

import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker

/**
 * #(v) (cardinality, or length (since a SeqValue can only contain at most one instance of any int value)
 * @param v is a SeqValue, containing a number of values, to count
 * @author renaud.delandtsheer@cetic.be
 */
case class Size(v: SeqValue)
  extends IntInvariant(v.value.size, 0 to DomainHelper.safeAddMax(v.max,1))
  with SeqNotificationTarget{

  setName("Size(" + v.name + ")")

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    this := changes.newValue.size
  }

  override def checkInternals(c: Checker) {
    c.check(this.value == v.value.size, Some("this.value == v.value.size"))
  }
}

