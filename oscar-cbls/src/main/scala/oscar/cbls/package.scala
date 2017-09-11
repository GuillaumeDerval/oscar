package oscar

import oscar.cbls.algo.search.InstrumentedRange
import oscar.cbls.core.computation._
import oscar.cbls.lib.constraint.{NE, EQ, GE, LE}
import oscar.cbls.lib.invariant.logic._
import oscar.cbls.lib.invariant.numeric._
import oscar.cbls.lib.invariant.set._
import oscar.cbls.modeling.{Searches, Combinators, Constraints, Invariants}

import scala.language.implicitConversions

/**
 * Created by rdl on 08-09-17.
 */
package object cbls extends Constraints with Invariants with Searches with Combinators{
  // Alias to useful classes and companion objects

  type Store = oscar.cbls.core.computation.Store
  final val Store = oscar.cbls.core.computation.Store

  type ErrorChecker = oscar.cbls.core.propagation.ErrorChecker
  final val ErrorChecker = oscar.cbls.core.propagation.ErrorChecker

  type Domain = oscar.cbls.core.computation.Domain
  final val Domain = oscar.cbls.core.computation.Domain

  final val fullRange = oscar.cbls.core.computation.FullRange
  final val positiveOrNullRange = oscar.cbls.core.computation.PositiveOrNullRange

  type Value = oscar.cbls.core.computation.Value
  type Variable = oscar.cbls.core.computation.Variable

  type Solution = oscar.cbls.core.computation.Solution
  type Snapshot = oscar.cbls.core.computation.Snapshot

  type AbstractVariable= oscar.cbls.core.computation.AbstractVariable

  type CascadingObjective = oscar.cbls.core.objective.CascadingObjective

  // Alias to useful classes and companion objects
  type CBLSIntVar = oscar.cbls.core.computation.CBLSIntVar
  final val CBLSIntVar = oscar.cbls.core.computation.CBLSIntVar

  //integer types
  type IntValue = oscar.cbls.core.computation.IntValue
  final val IntValue = oscar.cbls.core.computation.IntValue

  type CBLSIntConst = oscar.cbls.core.computation.CBLSIntConst
  final val CBLSIntConst = oscar.cbls.core.computation.CBLSIntConst

  implicit def intToCBLSIntConst(i:Int):IntValue = CBLSIntConst(i)

  //set types
  type CBLSSetVar = oscar.cbls.core.computation.CBLSSetVar
  final val CBLSSetVar = oscar.cbls.core.computation.CBLSSetVar

  type SetValue = oscar.cbls.core.computation.SetValue
  final val SetValue = oscar.cbls.core.computation.SetValue

  type CBLSSetConst = oscar.cbls.core.computation.CBLSSetConst
  final val CBLSSetConst = oscar.cbls.core.computation.CBLSSetConst

  //seq types
  type CBLSSeqVar = oscar.cbls.core.computation.CBLSSeqVar
  final val CBLSSeqVar = oscar.cbls.core.computation.CBLSSeqVar

  type SeqValue = oscar.cbls.core.computation.SeqValue
  type CBLSSeqConst = oscar.cbls.core.computation.CBLSSeqConst
  final val CBLSSeqConst = oscar.cbls.core.computation.CBLSSeqConst

  //objective types

  type Objective = oscar.cbls.core.objective.Objective
  final val Objective = oscar.cbls.core.objective.Objective

  //constraint types
  type Constraint = oscar.cbls.core.constraint.Constraint

  type ConstraintSystem = oscar.cbls.core.constraint.ConstraintSystem
  final val ConstraintSystem = oscar.cbls.core.constraint.ConstraintSystem

  //implicits
  implicit def intToIntVarOps(i:Int):IntVarOps = IntVarOps(CBLSIntConst(i))

  //some implicit for the CBLS variables, to add infix operators
  implicit class IntVarOps(x: IntValue) {
    def +(v: IntValue): IntInvariant = Sum2(x, v)

    def -(v: IntValue): IntInvariant = Minus(x, v)

    def *(v: IntValue): IntInvariant = Prod(List(x, v))

    def /(v: IntValue): IntInvariant = Div(x, v)
    def /(i:Int):IntInvariant = new Int2Int(x,_/i)

    def %(v: IntValue): IntInvariant = Mod(x, v)

    def le(b:IntValue) = LE(x,b)
    def ge(b:IntValue) = GE(x,b)

    def ===(b:IntValue) = EQ(x,b)
    def !==(b:IntValue) = NE(x,b)
    //we do not use the infix notation for comparison operators
    // because they are ambiguous with the <== operation of CBLS that is the "follows" operator)
  }

  implicit class SetVarOps(x: SetValue) {
    def union(v: SetValue): SetInvariant = Union(x, v)

    def inter(v: SetValue): SetInvariant = Inter(x, v)

    def minus(v: SetValue): SetInvariant = Diff(x, v)

    def map(fun: Int => Int, myMin: Int = Int.MinValue, myMax: Int = Int.MaxValue) = SetMap(x, fun, myMin to myMax)
  }

  implicit class IntValueArrayOps(intValueArray: Array[IntValue]) {
    def element(index:IntValue) = IntElement(index, intValueArray)

    def elements(index:SetValue) = Elements(index, intValueArray)
  }

  implicit class SetValueOps[X<:SetValue](setArray: Array[X]) {
    def element(index:IntValue): SetInvariant = SetElement(index, setArray)
  }

  implicit class IntArrayOps(intArray: Array[Int]) {
    def element(index:CBLSIntVar) = ConstantIntElement(index, intArray)

    def elements(index:SetValue) = Elements(index, intArray.map(CBLSIntConst.apply))
  }

  implicit def cBLSIntVarArrayToCBLSIntValueArray(a:Array[CBLSIntVar]):Array[IntValue] = {
    a.asInstanceOf[Array[IntValue]]
  }

  implicit def cBLSIntVarArrayToCBLSIntValueOpsArray(a:Array[CBLSIntVar]):IntValueArrayOps = {
    IntValueArrayOps(a.asInstanceOf[Array[IntValue]])
  }

  implicit def cBLSetVarArrayToCBLSSetValueArray(a:Array[CBLSSetVar]):Array[SetValue] = {
    a.asInstanceOf[Array[SetValue]]
  }

  // implicit conversion of Range towards a RangeHotRestart, to use the StartBy keyword
  implicit def instrumentRange(r: Range): InstrumentedRange = new InstrumentedRange(r)
}
