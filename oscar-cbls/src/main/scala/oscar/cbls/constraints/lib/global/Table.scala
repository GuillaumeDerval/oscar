package oscar.cbls.constraints.lib.global

import oscar.cbls.constraints.core.Constraint
import oscar.cbls.constraints.lib.basic.EQ
import oscar.cbls.invariants.core.computation.{Value, _}
import oscar.cbls.invariants.lib.minmax.{ArgMin, MinArray}
import oscar.cbls.invariants.lib.numeric.{Dist, Sum}

/**
  * Created by gustavbjordal on 08/06/16.
  */
case class Table(variables: Array[IntValue], table:Array[Array[IntValue]]) extends Invariant with Constraint with IntNotificationTarget {

  registerStaticAndDynamicDependencyAllNoID(variables)
  registerConstrainedVariables(variables)

  finishInitialization()

  val violationTable = Array.tabulate(table.size)(i =>
    Array.tabulate(variables.size)( j => Dist(variables(j), table(i)(j))
    )
  )

  val rowViolation:Array[IntValue] = Array.tabulate(table.size)(i =>
    Sum(violationTable(i))
  )

  val minViolatingRows = ArgMin(rowViolation)

  /** returns the violation associated with variable v in this constraint
    * all variables that are declared as constraint should have an associated violation degree.
    * notice that you cannot create any new invariant or variable in this method
    * because they can only be created before the model is closed.
    * */
  override def violation(v: Value): IntValue = {
    val aRow = minViolatingRows.value.head
    val variablesIndex = variables.indexOf(v)
    if(variablesIndex >= 0){
      violationTable(aRow)(variablesIndex)
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

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit = ???
}
