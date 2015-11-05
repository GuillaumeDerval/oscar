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
/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/


package oscar.cbls.constraints.lib.global

import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.core.computation.CBLSIntVar._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.modeling.Algebra._

import scala.collection.immutable.SortedMap

/**
 * Implement the AllDiff constraint on IntVars: all variables must have a different value.
 * in ase the same variable occurs several times (in case you submit a list of CBLSIntVar) these two occurrences
 * will be supposed to have different values as expected by the semantics of this constraint
 * @param variables the variable whose values should all be different.
 * @author renaud.delandtsheer@cetic.be
 */
case class AllDiff(variables: Iterable[IntValue]) extends Invariant with Constraint{

  registerStaticAndDynamicDependencyAllNoID(variables)
  registerConstrainedVariables(variables)
  finishInitialization()
  val (minValueOfVars,maxValueOfVars) = InvariantHelper.getMinMaxBounds(variables)

  //le degre global de violation est la somme des tailles -1 des ensembles de var ayant meme value
  // et on ne prend que les ensembles de cardinalite > 1
  private val Violation: CBLSIntVar = new CBLSIntVar(model, 0, (0 to variables.size), "ViolationsOfAllDiff")
  Violation.setDefiningInvariant(this)


  private val N0: Int = maxValueOfVars

  private val offset: Int = -minValueOfVars

  private val N = N0 + offset
  private val range = 0 to N

  private val ValueCount: Array[CBLSIntVar] = Array.tabulate[CBLSIntVar](N + 1)((i: Int) => {
    val tmp = new CBLSIntVar(model,0, (0 to variables.size), "alldiff_count_of_value_" + (i - offset))
    tmp.setDefiningInvariant(this)
    tmp
  })

  for (v <- variables) {
    ValueCount(v.value + offset) :+= 1
  }

  for (i <- range) {
    val tmp = ValueCount(i).getValue(true) - 1
    if (tmp > 0) Violation :+= tmp
  }

  /**the degree of violation of a variable is the number of other variables that have the same value as it. */
  private val Violations: SortedMap[IntValue, IntValue] = {
    def accumulate(acc:SortedMap[IntValue,IntValue], variable:IntValue, violation:IntValue):SortedMap[IntValue,IntValue] =
      acc + (acc.get(variable) match{
        case Some(oldViolation) => (variable,violation + oldViolation)
        case None => (variable,violation)})

    variables.foldLeft(
      SortedMap.empty[IntValue, IntValue])(
        (acc, intvar) => {
          val newvar = ValueCount.element((intvar + offset)) - 1
          accumulate(acc , intvar, newvar)
        })
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, OldVal: Int, NewVal: Int) {
    ValueCount(OldVal + offset) :-= 1
    ValueCount(NewVal + offset) :+= 1

    val DeltaOldVal = (if (ValueCount(OldVal + offset).getValue(true) == 0) 0 else -1)
    val DeltaNewVal = (if (ValueCount(NewVal + offset).getValue(true) == 1) 0 else 1)
    Violation :+= (DeltaNewVal + DeltaOldVal)
  }

  /**
   * The degree of violation of this constraint is the number of variables that should be changed
   * to ensure that the constraint is not violated.
   * @return an IntVar that can be incorporated in an invariant.
   */
  override def violation = Violation

  /**
   * The degree of violation of a variable is the number of other variables that have the same value
   * @return an IntVar that can be incorporated in an invariant.
   */
  override def violation(v: Value): IntValue = {
    val tmp: IntValue = Violations.getOrElse(v.asInstanceOf[IntValue], null)
    assert(tmp != null)
    tmp
  }

  override def checkInternals(c: Checker) {
    var MyValueCount: Array[Int] = (for (i <- 0 to N) yield 0).toArray
    for (v <- variables) MyValueCount(v.value + offset) += 1
    for (v <- range) {
      c.check(ValueCount(v).getValue(true) == MyValueCount(v),
        Some("ValueCount(" + v + ").getValue(true) (" + ValueCount(v).getValue(true)
          + ") == MyValueCount(" + v + ") (" + MyValueCount(v)))
    }

    for (v <- variables) {
      c.check(violation(v).value == MyValueCount(v.value + offset) - 1,
        Some("violation(" + v.name + ").value (" + violation(v).value
          + ") != MyValueCount(" + v.name + ".value + offset) - 1 ("
          + (MyValueCount(v.value + offset) - 1) + ")"))
    }

    var MyViol: Int = 0
    for (v <- range) MyViol += 0.max(MyValueCount(v) - 1)
    c.check(MyViol == Violation.value, Some("MyViol (" + MyViol
        + ") == Violation.value (" + Violation.value + ")"))
  }
}
