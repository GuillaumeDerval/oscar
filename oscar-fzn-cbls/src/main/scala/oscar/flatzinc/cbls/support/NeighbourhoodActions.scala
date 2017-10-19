/*
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

package oscar.flatzinc.cbls.support

import oscar.cbls.core.computation.{CBLSIntVar, IntValue}

/**
  * @author Gustav Bjordal
  */
abstract class NeighbourhoodAction {
  def computeAssignment()

  def performAssignment()

  def unroll()

  def saveBest()

  def getOldMove(obj: Int): Move

  def onlyModifies(allowedVars: CBLSIntVar => Boolean):Boolean
}

case class AssignAction(target: CBLSIntVar, value: IntValue) extends NeighbourhoodAction {
  var computedValue: Int = 0
  var oldValue: Int = 0
  var savedValue: Int = 0

  override def computeAssignment(): Unit = {
    computedValue = value.value
    oldValue = target.value
  }


  override def saveBest(): Unit = {
    savedValue = computedValue
  }

  override def performAssignment(): Unit = {
    target := computedValue
  }

  override def unroll(): Unit = {
    target := oldValue
  }

  override def getOldMove(obj: Int): Move = {
    AssignMove(target, savedValue, obj)
  }

  override def onlyModifies(allowedVars: (CBLSIntVar) => Boolean): Boolean = {
    allowedVars(target)
  }
}

case class AssignArrayAction(targets: Array[CBLSIntVar], index: IntValue,
                             value: IntValue) extends NeighbourhoodAction {
  var computedValue: Int = 0
  var computedIndex: Int = 0
  var oldValue: Int = 0

  var savedValue: Int = 0
  var savedIndex: Int = 0

  override def computeAssignment(): Unit = {
    computedValue = value.value
    computedIndex = index.value-1
    oldValue = targets(computedIndex).value
  }


  override def saveBest(): Unit = {
    savedValue = computedValue
    savedIndex = computedIndex
  }

  override def performAssignment(): Unit = {
    targets(computedIndex) := computedValue
  }

  override def unroll(): Unit = {
    targets(computedIndex) := oldValue
  }

  override def getOldMove(obj: Int): Move = {
    AssignMove(targets(savedIndex), savedValue, obj)
  }

  override def onlyModifies(allowedVars: (CBLSIntVar) => Boolean): Boolean = {
    allowedVars(targets(computedIndex))
  }
}

case class SwapAction(target1: CBLSIntVar, target2: CBLSIntVar) extends NeighbourhoodAction {
  override def computeAssignment(): Unit = {}


  override def saveBest(): Unit = {}

  override def performAssignment(): Unit = {
    target1 :=: target2
  }

  override def unroll(): Unit = {
    target1 :=: target2
  }

  override def getOldMove(obj: Int): Move = {
    SwapMove(target1, target2, obj)
  }
  override def onlyModifies(allowedVars: (CBLSIntVar) => Boolean): Boolean = {
    allowedVars(target1) && allowedVars(target2)
  }
}

case class SwapArrayAction(targets1: Array[CBLSIntVar],
                           index1: IntValue,
                           targets2: Array[CBLSIntVar],
                           index2: IntValue) extends NeighbourhoodAction {
  var computedIndex1: Int = 0
  var computedIndex2: Int = 0
  var savedIndex1: Int = 0
  var savedIndex2: Int = 0

  override def computeAssignment(): Unit = {
    computedIndex1 = index1.value-1
    computedIndex2 = index2.value-1
  }


  override def saveBest(): Unit = {
    savedIndex1 = computedIndex1
    savedIndex2 = computedIndex2
  }

  override def performAssignment(): Unit = {
    targets1(computedIndex1) :=: targets1(computedIndex2)
  }

  override def unroll(): Unit = {

    targets1(computedIndex1) :=: targets1(computedIndex2)
  }

  override def getOldMove(obj: Int): Move = {
    SwapMove(targets1(savedIndex1), targets2(savedIndex2), obj)
  }
  override def onlyModifies(allowedVars: (CBLSIntVar) => Boolean): Boolean = {
    allowedVars(targets1(computedIndex1)) && allowedVars(targets2(computedIndex2))
  }
}
