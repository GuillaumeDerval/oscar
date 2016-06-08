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
/**
  * @author Gustav Björdal
  *
  **/

package oscar.cbls.constraints.lib.global

import oscar.cbls.constraints.core.Constraint
import oscar.cbls.constraints.lib.basic.EQ
import oscar.cbls.invariants.core.computation.{Value, _}
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.lib.logic.IntElement
import oscar.cbls.invariants.lib.minmax.{ArgMin, MinArray}
import oscar.cbls.invariants.lib.numeric._
import oscar.cbls.invariants.lib.set.TakeAny

/**
  * Implementation of the table constraint. For each row, the violation of the row is the
  * number of elements that are different from the corresponding variable in variables.
  * The violation of the constraint is the minimum violation of any row.
  * 
  * @param variables the variables.
  * @param table an array of the rows of the table.
  * @author gustav.bjordal@it.uu.se
  */

case class Table(variables: List[IntValue], table:Array[Array[Int]]) extends Invariant with Constraint with IntNotificationTarget{

  registerStaticAndDynamicDependencyAllNoID(variables)
  registerConstrainedVariables(variables)

  finishInitialization()

  val violationTable = Array.tabulate(table.size)(i =>
    Array.tabulate(variables.size)( j => Step(EQ(variables(j), table(i)(j)).violation,0,1,0)
    )
  )

  val rowViolation:Array[IntValue] = Array.tabulate(table.size)(i =>
    Sum(violationTable(i))
  )

  val minViolatingRows = ArgMin(rowViolation)

  val aMinViolatingRow = TakeAny(minViolatingRows,0)

  val variableViolation = Array.tabulate(variables.size)(i =>
    IntElement(aMinViolatingRow,violationTable.map(_(i)))
  )
  /** returns the violation associated with variable v in this constraint
    * all variables that are declared as constraint should have an associated violation degree.
    * notice that you cannot create any new invariant or variable in this method
    * because they can only be created before the model is closed.
    * */
  override def violation(v: Value): IntValue = {
    val variablesIndex = variables.indexOf(v)
    if(variablesIndex >= 0){
      variableViolation(variablesIndex)
    }else{
      0
    }
  }
  val minViolation = MinArray(rowViolation)
  /** returns the degree of violation of the constraint
    * notice that you cannot create any new invariant or variable in this method
    * because they can only be created before the model is closed.
    *
    * @return
    */
  override def violation: IntValue = minViolation

  override def checkInternals(c: Checker) {

  }
 @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit = {
  }
}
