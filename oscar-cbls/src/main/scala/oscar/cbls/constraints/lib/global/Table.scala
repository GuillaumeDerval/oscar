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
  * Created by gustavbjordal on 08/06/16.
  */
case class Table(variables: List[ChangingIntValue], table:Array[Array[Int]]) extends Invariant with Constraint with IntNotificationTarget{

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

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit = {
    for(r <- violationTable)
      println(r.mkString(", "))
  }
}
