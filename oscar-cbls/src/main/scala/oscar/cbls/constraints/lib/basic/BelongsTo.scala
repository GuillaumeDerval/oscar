package oscar.cbls.constraints.lib.basic

import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

/**
 * implements v \in set
 * @author renaud.delandtsheer@cetic.be
 */
case class BelongsTo(v: IntValue, set: SetValue) extends Invariant with Constraint {
  registerConstrainedVariables(v, set)
  registerStaticAndDynamicDependenciesNoID(v, set)
  finishInitialization()

  /** the violation is 1 if v is not in set, 0 otherwise*/
  override val violation: CBLSIntVar = CBLSIntVar(model, if (set.value.contains(v.value)) 0 else 1, 0 to 1, "belongsTo(" + v.name + "," + set.name + ")")

  violation.setDefiningInvariant(this)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, OldVal: Int, NewVal: Int) {
    violation := (if (set.value.contains(v.value)) 0 else 1)
  }

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    if (this.v.value == value) violation := 0
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    if (this.v.value == value) violation := 1
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
