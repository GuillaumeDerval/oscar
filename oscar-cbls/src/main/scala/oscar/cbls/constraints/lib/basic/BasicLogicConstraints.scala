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
/**
 * ****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 * ****************************************************************************
 */

package oscar.cbls.constraints.lib.basic

import oscar.cbls.constraints.core._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.lib.logic.IntInt2Int
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.invariants.lib.numeric.Dist
import oscar.cbls.modeling.Algebra._
import scala.math.abs
import oscar.cbls.invariants.lib.logic.IntInt2Int
import oscar.cbls.invariants.lib.numeric.MinusOffsetPos
import oscar.cbls.invariants.lib.numeric.ReifViol

/**
 * implements left <= right
 * @author renaud.delandtsheer@cetic.be
 */
protected class LEA(val left: IntValue, val right: IntValue) extends Constraint {
  val model = InvariantHelper.findModel(left, right)

  registerConstrainedVariables(left, right)

  /**
   * the violation is Max(0,right-left)
   */
  override val violation =
    MinusOffsetPos(left,right,0).setName(this.getClass().getSimpleName() + ".violation")
    //Max2(0, left - right).setName(this.getClass().getSimpleName() + ".violation")

  /**
   * The violation of each variable is equal to the global violation of the constraint
   */
  override def violation(v: Value): IntValue = { if (left == v || right == v) violation else 0 }

  override def checkInternals(c: Checker) {
    val diff = left.value - right.value
    c.check(violation.value == (if (diff <= 0) 0 else diff),
      Some("Violation.value (" + violation.value
        + ") == (if (left.value - right.value (" + diff + ") <= 0) 0 else " + diff + ")"))
  }
}

/**
 * implements left <= right
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class LE(l: IntValue, r: IntValue) extends LEA(l, r)

/**
 * implements left >= right
 * it is just a parameter swap of [[oscar.cbls.constraints.lib.basic.LE]]
 * @author renaud.delandtsheer@cetic.be
 */
case class GE(l: IntValue, r: IntValue) extends LEA(r, l)

/**
 * implements left < right
 * @author renaud.delandtsheer@cetic.be
 */
protected class LA(val left: IntValue, val right: IntValue) extends Constraint {
  registerConstrainedVariables(left, right)

  /**
   * the violation is Max(0,left - right + 1)
   */
  override val violation =
    MinusOffsetPos(left,right,1).setName(this.getClass().getSimpleName() + ".violation")
    //TODO: If the constraint is always satisfied, given the domains, should set to a constant invariant. 
    //Max2(0, left - right + 1)

  /**
   * The violation of each variable is equal to the global violation of the constraint
   */
  override def violation(v: Value): IntValue = { if (left == v || right == v) violation else 0 }

  override def checkInternals(c: Checker) {
    val diff = left.value - right.value
    c.check(violation.value == (if (diff < 0) 0 else diff + 1),
      Some("Violation.value (" + violation.value
        + ") == (if (left.value - right.value (" + diff + ") < 0) 0 else " + (diff + 1) + ")"))
  }
}

/**
 * implements left < right
 * it is just a parameter swap of [[oscar.cbls.constraints.lib.basic.L]]
 * @author renaud.delandtsheer@cetic.be
 */
case class L(l: IntValue, r: IntValue) extends LA(l, r)

/**
 * implements left > right
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class G(l: IntValue, r: IntValue) extends LA(r, l)

/**
 * implements left != right
 * @author renaud.delandtsheer@cetic.be
 */
case class NE(left: IntValue, right: IntValue) extends Invariant with Constraint {
  registerConstrainedVariables(left, right)
  registerStaticAndDynamicDependenciesNoID(left, right)
  finishInitialization()

  /** the violation is 1 if the variables are equal, 0 otherwise*/
  override val violation: CBLSIntVar = CBLSIntVar(model, if (left.value == right.value) 1 else 0, 0 to 1, "equals")

  violation.setDefiningInvariant(this)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, OldVal: Int, NewVal: Int) {
    violation := (if (left.value == right.value) 1 else 0)
  }

  /** the violation is 1 if the variables are equal, 0 otherwise*/
  override def violation(v: Value): IntValue = { if (left == v || right == v) violation else 0 }

  override def checkInternals(c: Checker) {
    c.check(violation.value == (if (left.value == right.value) 1 else 0),
      Some("Violation.value (" + violation.value
        + ") == (if (left.value (" + left.value + ") == right.value (" + right.value + ")) 1 else 0)"))
  }
}

/**
 * constraints left == right
 * this is considered as a primitive constraint and used in the [[oscar.cbls.constraints.core.Constraint]]
 * class, so that it is part of the core instead of the library
 * @author renaud.delandtsheer@cetic.be
 */
case class EQ(left: IntValue, right: IntValue) extends Constraint {

  registerConstrainedVariables(left, right)

  override val violation = Dist(left, right)

  override def violation(v: Value) = { if (left == v || right == v) violation else 0 }

  override def checkInternals(c: Checker) {
    val myViolation = abs(left.value - right.value)
    c.check(violation.value == (if (left.value == right.value) 0 else myViolation),
      Some("Violation.value (" + violation.value
        + ") == (if (left.value (" + left.value + ") == right.value (" + right.value
        + ")) 0 else " + myViolation + ")"))
  }
}
/**
 * constraints b <=> c, i.e., b is true iff c is satisfied.
 * @author jean-noel.monette@it.uu.se 
 */
case class Reif(b: IntValue, c: Constraint) extends Constraint { 
  registerConstrainedVariables(b, c.violation)
  override val violation = ReifViol(b,c.violation)
  override def violation(v: Value) =  { if (b == v || c.violation == v) violation else 0 }
  //TODO: Check internals
}
