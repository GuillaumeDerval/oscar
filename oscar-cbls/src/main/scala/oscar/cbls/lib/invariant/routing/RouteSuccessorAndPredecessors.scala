package oscar.cbls.lib.invariant.routing

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

import oscar.cbls.algo.seq.functional.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.routing.convention.RoutingConventionMethods

import scala.collection.immutable.SortedSet

object RouteSuccessorAndPredecessors{
  def apply(routes:ChangingSeqValue,
            v:Int,
            defaultWhenNotInSequence:Int):(Array[CBLSIntVar],Array[CBLSIntVar]) = {
    val n = routes.maxValue + 1
    val model = routes.model
    val successorVars = Array.tabulate(n)(node =>  CBLSIntVar(model,defaultWhenNotInSequence,name="successor of node" + node))
    val predecessorVars = Array.tabulate(n)(node =>  CBLSIntVar(model,defaultWhenNotInSequence,name="predecessor of node" + node))

    new RouteSuccessorAndPredecessors(routes, v, successorVars,predecessorVars,defaultWhenNotInSequence)

    (successorVars,predecessorVars)
  }
}

class RouteSuccessorAndPredecessors(routes:ChangingSeqValue,
                     v:Int,
                     successorValues:Array[CBLSIntVar],
                     predecessorValues:Array[CBLSIntVar],
                     defaultWhenNotInSequence:Int)
  extends Invariant() with SeqNotificationTarget {

  val n = routes.maxValue + 1
  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for(i <- successorValues) i.setDefiningInvariant(this)
  for(i <- predecessorValues) i.setDefiningInvariant(this)

  computeAllFromScratch(routes.value)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {

    val startValuesOfImpactedZone = computeStartValuesOfImpactedZone(changes:SeqUpdate)

    startValuesOfImpactedZone match{
      case None =>  computeAllFromScratch(changes.newValue)
      case Some(startUpdateValues) =>
        val startUpdateExplorersAndVal = startUpdateValues.toList.map(value => (value,changes.newValue.explorerAtAnyOccurrence(value)))
        val startUpdateExplorersAndValSortedByPos =
          startUpdateExplorersAndVal.sortBy(valueAndExplorer => valueAndExplorer._2 match{
            case None => -valueAndExplorer._1
            case Some(explorer) => explorer.position})

        startUpdateExplorersAndValSortedByPos.foreach(startValueAndExplorer =>
          updateStartFrom(startValueAndExplorer._1,startValueAndExplorer._2,changes.newValue))
    }
  }

  def computeStartValuesOfImpactedZone(changes:SeqUpdate):Option[SortedSet[Int]] = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        computeStartValuesOfImpactedZone(prev) match{
          case None => None
          case Some(startsOfImpactedZone) =>
            Some(startsOfImpactedZone + value + RoutingConventionMethods.routingPredVal2Val(value,s.newValue,v))
        }

      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        computeStartValuesOfImpactedZone(prev) match{
          case None => None
          case Some(startsOfImpactedZone) => Some(
            startsOfImpactedZone +
              RoutingConventionMethods.routingPredPos2Val(fromIncluded,prev.newValue,v) +
              m.fromValue + m.toValue + m.afterValue)
        }

      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        val removedValue = r.removedValue
        computeStartValuesOfImpactedZone(prev) match{
          case None => None
          case Some(startsOfImpactedZone) => Some(
            startsOfImpactedZone + removedValue +
              RoutingConventionMethods.routingPredPos2Val(position,prev.newValue,v) )
        }

      case SeqUpdateLastNotified(value) =>
        require (value quickEquals routes.value)
        Some(SortedSet.empty[Int]) //we are starting from the previous value
      case SeqUpdateAssign(value : IntSequence) =>
        None //impossible to go incremental
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,_,_) =>
        computeStartValuesOfImpactedZone(prev)
      case u@SeqUpdateRollBackToCheckpoint(_,_) =>
        computeStartValuesOfImpactedZone(u.howToRollBack)
    }
  }

  def computeAllFromScratch(seq:IntSequence){
    successorValues.foreach(node => node := defaultWhenNotInSequence)
    predecessorValues.foreach(node => node := defaultWhenNotInSequence)
    var explorer = seq.explorerAtPosition(0).head
    while(explorer.next match{
      case None =>
        successorValues(explorer.value) := v-1
        predecessorValues(v-1) := explorer.value
        false
      case Some(next) =>
        if(next.value < v){
          successorValues(explorer.value) := next.value - 1
          predecessorValues(next.value - 1) := explorer.value
        }else{
          successorValues(explorer.value) := next.value
          predecessorValues(next.value) := explorer.value
        }
        explorer = next
        true
    }){}
  }

  def updateStartFrom(startValue:Int,startExplorerOpt:Option[IntSequenceExplorer],seq:IntSequence){
    startExplorerOpt match{
      case None =>
        successorValues(startValue) := defaultWhenNotInSequence
        predecessorValues(startValue) := defaultWhenNotInSequence
      case Some(startExplorer) =>
        var explorer = startExplorer
        while(explorer.next match{
          case None =>
            successorValues(explorer.value) := v-1
            predecessorValues(v-1) := explorer.value
            false
          case Some(next) =>
            val newValueForSuccValue =
              if(next.value < v) next.value - 1
              else next.value
            if(successorValues(explorer.value).newValue != newValueForSuccValue){
              successorValues(explorer.value) := newValueForSuccValue
              predecessorValues(newValueForSuccValue) := explorer.value
              explorer = next
              true
            }else false
        }){}
    }
  }

  def computeSuccessorsFromScratchNoAffect(seq:IntSequence):Array[Int] = {
    val successorValues = Array.fill(n)(defaultWhenNotInSequence)

    var explorer = seq.explorerAtPosition(0).head
    while(explorer.next match{
      case None =>
        successorValues(explorer.value) = v-1
        false
      case Some(next) =>
        if(next.value < v){
          successorValues(explorer.value) = next.value - 1
        }else{
          successorValues(explorer.value) = next.value
        }
        explorer = next
        true
    }){}
    successorValues
  }

  override def checkInternals() {
    val fromScratch = computeSuccessorsFromScratchNoAffect(routes.value)
    for(node <- 0 until n){
      require(successorValues(node).newValue == fromScratch(node),
        Some("error on next for node " + node + ": " + successorValues(node) + " should== " + fromScratch(node)))

      if(fromScratch(node)== defaultWhenNotInSequence){
        require(predecessorValues(node).newValue == defaultWhenNotInSequence,
          Some("error on predecessor for node " + node))
      }else {
        require(predecessorValues(fromScratch(node)).newValue == node,
          Some("error on predecessor for node " + node))
      }
    }
  }
}
