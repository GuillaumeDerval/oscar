package oscar.cbls.lib.constraint

/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

/**
 * implements v \in set
 * @author renaud.delandtsheer@cetic.be
 */
case class BelongsTo(v: IntValue, set: SetValue)
  extends Invariant
  with Constraint
  with IntNotificationTarget
  with SetNotificationTarget{

  registerConstrainedVariables(v, set)
  registerStaticAndDynamicDependenciesNoID(v, set)
  finishInitialization()

  /** the violation is 1 if v is not in set, 0 otherwise*/
  override val violation: CBLSIntVar = CBLSIntVar(model, if (set.value.contains(v.value)) 0 else 1, 0 to 1, "belongsTo(" + v.name + "," + set.name + ")")

  violation.setDefiningInvariant(this)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    violation := (if (set.value.contains(v.value)) 0 else 1)
  }

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    if (newValue.contains(this.v.value)) violation := 0
    else violation := 1
  }

  /** the violation is 1 v is not is set, 0 otherwise*/
  override def violation(v: Value): IntValue = { if (this.v == v || this.set == v) violation else 0 }

  /**
   * To override whenever possible to spot errors in invariants.
   * this will be called for each invariant after propagation is performed.
   * It requires that the Model is instantiated with the variable debug set to true.
   */
  override def checkInternals(c: Checker) {
    c.check(violation.value == (if (set.value.contains(v.value)) 0 else 1),
      Some("Violation.value (" + violation.value
        + ") == (if(set.value" + set.value + ".contains(v.value (" + v.value + "))) 0 else 1)"))
  }
}
