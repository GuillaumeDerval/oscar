package oscar.cbls.routing.neighborhood

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

import oscar.cbls.invariants.core.computation.{CBLSIntVar, Variable}
import oscar.cbls.routing.model.{HotSpottingInfo, VRP}
import oscar.cbls.search.core.EasyNeighborhood
import oscar.cbls.search.move.Move

abstract class VRPMove(override val objAfter: Int,
                       val neighborhood: EasyRoutingNeighborhood[_],
                       override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName) with HotSpottingInfo{

  /** to actually take the move */
  override def commit(){
    neighborhood.cleanRecordedMoves()
    encodeMove()
    neighborhood.commit(false)
  }

  override def touchedVariables: List[Variable] = impactedPoints.map(neighborhood.vrp.next(_))

  def encodeMove()
}

/**
 * describes moves in a spart way by use of segments
 * @author renaud.delandtsheer@cetic.be
 */
abstract class EasyRoutingNeighborhood[M<:Move](best:Boolean, val vrp:VRP, neighborhoodName:String) extends EasyNeighborhood[M](best,neighborhoodName) {
  private var Recording = true //recording ou comitted
  protected def isRecording = Recording
  protected def noMoveRecorded = affects.isEmpty

  private var affects: List[Affect] = List.empty

  @inline
  protected def evalObjOnEncodedMove():Int = {
    commit(true)
    val newObj = obj.value
    undo()
    newObj
  }

  protected def addMove(affect: Affect) {
    require(Recording)
    //println("addMove: " + affect)
    affects = affect :: affects
  }

  protected abstract class Affect(variableNode:Int) {
    def comit(): Affect
    def node: Int = variableNode
    def variable =  vrp.next(variableNode)
  }

  case class affectFromVariable(variableNode: Int, takeValueFromNode: Int) extends Affect(variableNode) {
    def comit(): Affect = {
      val variable = vrp.next(variableNode)
      val takeValueFrom = vrp.next(takeValueFromNode)
      val oldValue = variable.value
      assert(vrp.isRouted(takeValueFrom.value), "you cannot take the value of an unrouted variable " + variable + " take value from " + takeValueFrom)
      variable := takeValueFrom.value
      affectFromConst(variableNode, oldValue)
    }
  }

  case class affectFromConst(variableNode: Int, takeValue: Int) extends Affect(variableNode) {
    def comit(): Affect = {
      val variable = vrp.next(variableNode)
      assert(variable.value != vrp.N || takeValue != vrp.N, "you cannot unroute a node that is already unrouted " + variable)
      val oldValue = variable.value
      variable := takeValue
      affectFromConst(variableNode, oldValue)
    }
  }

  protected case class Segment(start: Int, end: Int)

  def cut(beforeStart: Int, end: Int): Segment = {
    addMove(affectFromVariable(beforeStart, end))
    Segment(vrp.next(beforeStart).value, end)
  }

  def cutNodeAfter(beforeStart: Int): Segment = {
    assert(vrp.isRouted(beforeStart), "you cannot cut after an unrouted node " + beforeStart)
    val start = vrp.next(beforeStart).value
    addMove(affectFromVariable(beforeStart, start))
    Segment(start, start)
  }

  def segmentFromUnrouted(n: Int): Segment = {
    assert(!vrp.isRouted(n), "you cannot make a segment from unrouted if node is actually routed " + n)
    Segment(n, n)
  }

  def reverse(s: Segment): Segment = {
    //println("reversing " + s)
    var prev = s.start
    var current: Int = vrp.next(prev).value
    while (prev != s.end) {
      addMove(affectFromConst(current, prev))
      prev = current
      current = vrp.next(current).value
    }
    //println("done")
    Segment(s.end, s.start)
  }

  /**
   * Reverse a routed segment in its right place.
   * segments are handled internally, so nothing is returned
   */
  def reverseSegmentInPlace(beforeStart: Int, segEndPoint: Int) {
    val seg = cut(beforeStart, segEndPoint)
    val revSeg = reverse(seg)
    insert(revSeg, beforeStart)
  }

  def insert(s: Segment, node: Int) {
    //println("inserting " + s + " after node " + node)
    addMove(affectFromVariable(s.end, node))
    addMove(affectFromConst(node, s.start))
  }

  def append(s: Segment, t: Segment): Segment = {
    addMove(affectFromConst(s.end, t.start))
    Segment(s.start, t.end)
  }

  def unroute(s: Segment) {
    def unroute(n: Int) {
      assert(n >= vrp.V, "you cannot unroute a depot: (depot=" + n + ")")
      addMove(affectFromConst(n, vrp.N))
    }
    var current = s.start
    unroute(current)
    while (current != s.end) {
      current = vrp.next(current).value
      unroute(current)
    }
  }

  def commit(recordForUndo: Boolean = false) {
    require(Recording, "MoveDescription should be recording now")
    if (recordForUndo) {
      affects = doAllMovesAndReturnRollBack()
      Recording = false
    } else {
      doAllMoves()
      affects = List.empty
    }
  }

  private def doAllMovesAndReturnRollBack(): List[Affect] = {
    var undoList: List[Affect] = List.empty
    def doIt(toDo: List[Affect]) {
      toDo match {
        case head :: tail =>
          doIt(tail)
          undoList = head.comit() :: undoList
        case Nil => ;
      }
    }
    doIt(affects)
    undoList.reverse //TODO: find a better way to get it.
  }

  private def doAllMoves() {
    def doIt(toDo: List[Affect]) {
      toDo match {
        case head :: tail =>
          doIt(tail); head.comit()
        case Nil => ;
      }
    }
    doIt(affects)
  }

  def undo() {
    require(!Recording, "MoveDescription should not be recording now")
    Recording = true
    commit(false)
    Recording = true
  }

  def cleanRecordedMoves() {
    affects = List.empty
    Recording = true
  }

  def touchedVariablesByEncodedMove:List[CBLSIntVar] = affects.map(_.variable)
  def touchedNodesByEncodedMove:List[Int] = affects.map(_.node)
}