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

package oscar.cbls.lib.invariant.set

import oscar.cbls.algo.quick.QList
import oscar.cbls.core.computation.{ChangingSetValue, SetNotificationTarget, IntInvariant, SetValue}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

/**
 * @param s
 * @param subsetToMonitorAndMaxValues iterable of (subset, max occurrence in the subset, weight in case of violation)
 */
case class IncludedSubsets(s: SetValue, subsetToMonitorAndMaxValues:Iterable[(Iterable[Int],Int,Int)])
  extends IntInvariant(0,0 to subsetToMonitorAndMaxValues.size)
  with SetNotificationTarget{

  registerStaticAndDynamicDependenciesNoID(s)
  finishInitialization()

  require(s.min >= 0, "restricting assumption on this invariant: s.min >= 0")

  val subsetAndMaxAndWeightArray = subsetToMonitorAndMaxValues.toArray
  val n = subsetAndMaxAndWeightArray.length

  //building valueToSubsetID
  val valueToSubsetID:Array[QList[Int]] = Array.fill(s.max+1)(null)
  for (forbiddenID <- subsetAndMaxAndWeightArray.indices) {
    val (values,maxNumber,weight) = subsetAndMaxAndWeightArray(forbiddenID)
    for(value <- values){
      valueToSubsetID(value) = QList(forbiddenID,valueToSubsetID(value))
    }
  }

  //internal value for quick update
  val subsetToNbPresent:Array[Int] = Array.fill(n)(0)

  //initializing
  this := 0
  s.value.foreach(notifyInsert)

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int],
                                removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    for (added <- addedValues) notifyInsert(added)
    for (deleted <- removedValues) notifyRemove(deleted)
  }

  @inline
  private def notifyInsert(value: Int) {
    for(subset <- QList.toIterable(valueToSubsetID(value))){
      subsetToNbPresent(subset) = subsetToNbPresent(subset) + 1
      if(subsetToNbPresent(subset) == subsetAndMaxAndWeightArray(subset)._2 + 1) {
        this :+= subsetAndMaxAndWeightArray(subset)._3
      }
    }
  }

  @inline
  private def notifyRemove(value: Int) {
    for(subset <- QList.toIterable(valueToSubsetID(value))){
      subsetToNbPresent(subset) = subsetToNbPresent(subset) - 1
      if(subsetToNbPresent(subset) == subsetAndMaxAndWeightArray(subset)._2){
        this :-= subsetAndMaxAndWeightArray(subset)._3
      }
    }
  }

  override def checkInternals(c: Checker) {
    val violation = subsetToMonitorAndMaxValues.map({case (values,maxValue,weight) => if(values.count(v => s.value.contains(v)) > maxValue) weight else 0}).sum
    c.check(this.value == violation,Some("included subset Error value=" + this.value  + " should be:" + violation))
  }
}

