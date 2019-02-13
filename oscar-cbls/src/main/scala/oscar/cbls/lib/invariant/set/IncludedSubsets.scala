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
import oscar.cbls._
import oscar.cbls.core._

import scala.collection.immutable.SortedSet

/**
 * sum(on s:subsetToMonitorAndMaxValues if #(s._1 inter s) > s._2 of s._3 )
 * @param s a setValue
 * @param subsetToMonitorAndMaxValues iterable of (subset, max occurrence in the subset, weight in case of violation)
 */
case class IncludedSubsets(s: SetValue, subsetToMonitorAndMaxValues:Iterable[(Iterable[Long],Long,Long)])
  extends IntInvariant(0L,Domain(0L , subsetToMonitorAndMaxValues.map(_._3).sum))
  with SetNotificationTarget{

  registerStaticAndDynamicDependenciesNoID(s)
  finishInitialization()

  require(s.min == 0L, "SetValue min domain should be equal to 0L instead of " + s.min)

  val subsetAndMaxAndWeightArray = subsetToMonitorAndMaxValues.toArray
  val n = subsetAndMaxAndWeightArray.length

  val valueToSubsetID:Array[QList[Long]] = Array.fill(s.max+1L)(null)

  for (forbiddenID <- subsetAndMaxAndWeightArray.indices) {
    val (values,maxNumber,weight) = subsetAndMaxAndWeightArray(forbiddenID)
    for(value <- values){
      valueToSubsetID(value) = QList(forbiddenID,valueToSubsetID(value))
    }
  }

  this := 0L
  val subsetToNbPresent = Array.fill[Long](n)(0)
  for(value <- s.value){
    notifyInsert(value)
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Long], removedValues: Iterable[Long], oldValue: SortedSet[Long], newValue: SortedSet[Long]): Unit = {
    for (added <- addedValues) notifyInsert(added)
    for (deleted <- removedValues) notifyDelete(deleted)
  }

  @inline
  private def notifyInsert(value: Long) {
    for(subset <- QList.toIterable(valueToSubsetID(value))){
      subsetToNbPresent(subset) = subsetToNbPresent(subset) + 1L
      if(subsetToNbPresent(subset) == subsetAndMaxAndWeightArray(subset)._2 + 1L) {
        this :+= subsetAndMaxAndWeightArray(subset)._3
      }
    }
  }

  @inline
  private def notifyDelete(value: Long) {
    for(subset <- QList.toIterable(valueToSubsetID(value))){
      subsetToNbPresent(subset) = subsetToNbPresent(subset) - 1L
      if(subsetToNbPresent(subset) == subsetAndMaxAndWeightArray(subset)._2){
        this :-= subsetAndMaxAndWeightArray(subset)._3
      }
    }
  }

  override def checkInternals(c: Checker) {
    val violation = subsetToMonitorAndMaxValues.map({case (values,maxValue,weight) => if(values.count(v => s.value.contains(v)) > maxValue) weight else 0L}).sum
    c.check(this.value == violation,Some("included subset Error value=" + this.value  + " should be:" + violation))
  }
}


/**
 * {i in such that there is a clause {subset,maxOcc} in clauseAndMaxOccList such that #(subset \inter s) > maxOcc)
 * @param s a setValue
 * @param clauseAndMaxOccList iterable of (subset, max occurrence in the subset)
 */
case class ValuesInViolatedClauses(s: SetValue, clauseAndMaxOccList:Iterable[(Iterable[Long],Long)])
  extends SetInvariant(initialDomain = s.domain)
  with SetNotificationTarget{

  registerStaticAndDynamicDependenciesNoID(s)
  finishInitialization()

  require(s.min == 0L)

  val clauseAndMaxOccArray = clauseAndMaxOccList.toArray
  val numberOfClauses = clauseAndMaxOccArray.length

  //building valueToClauseIDs
  val valueToClauseIDs:Array[QList[Long]] = Array.fill(s.max+1L)(null)
  for (clauseID <- clauseAndMaxOccArray.indices) {
    val (values,maxNumber) = clauseAndMaxOccArray(clauseID)
    for(value <- values){
      valueToClauseIDs(value) = QList(clauseID,valueToClauseIDs(value))
    }
  }

  //value to number of violated clauses this is in
  val valToNumberOfViolatedClauses:Array[Long] = Array.fill(s.max + 1L)(0L)

  //clause to number of values in s in it
  val clauseToNbPresent:Array[Long] = Array.fill(numberOfClauses)(0L)

  //init
  this := SortedSet.empty
  for(value <- s.value){
    notifyInsert(value)
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Long], removedValues: Iterable[Long], oldValue: SortedSet[Long], newValue: SortedSet[Long]): Unit = {
    for (added <- addedValues) notifyInsert(added)
    for (deleted <- removedValues) notifyDelete(deleted)
  }

  @inline
  private def setClauseViolated(clauseNumber:Long){
    for(number <- clauseAndMaxOccArray(clauseNumber)._1){
      val oldCount = valToNumberOfViolatedClauses(number)
      valToNumberOfViolatedClauses(number) = oldCount + 1L
      if(oldCount == 0L){
        this.insertValue(number)
      }
    }
  }

  @inline
  private def setClauseNonViolated(clauseNumber:Long){
    for(number <- clauseAndMaxOccArray(clauseNumber)._1){
      val oldCount = valToNumberOfViolatedClauses(number)
      valToNumberOfViolatedClauses(number) = oldCount - 1L
      if(oldCount == 1L){
        this.deleteValue(number)
      }
    }
  }

  @inline
  private def notifyInsert(value: Long) {
    for(clauseNumber <- QList.toIterable(valueToClauseIDs(value))){
      val oldNbPresent = clauseToNbPresent(clauseNumber)
      clauseToNbPresent(clauseNumber) = oldNbPresent + 1L
      if(oldNbPresent == clauseAndMaxOccArray(clauseNumber)._2) {
        setClauseViolated(clauseNumber)
      }
    }
  }

  @inline
  private def notifyDelete(value: Long) {
    for(clauseNumber <- QList.toIterable(valueToClauseIDs(value))){
      val oldNbPresent = clauseToNbPresent(clauseNumber)
      clauseToNbPresent(clauseNumber) = oldNbPresent - 1L
      if(oldNbPresent == clauseAndMaxOccArray(clauseNumber)._2 + 1L){
        setClauseNonViolated(clauseNumber)
      }
    }
  }

  private def computeFromScratch(values:SortedSet[Long]):SortedSet[Long] = {
    val violatedClauses = clauseAndMaxOccList.filter({case (clause,maxOcc) => clause.count(v => values.contains(v)) > maxOcc})
    SortedSet.empty[Long] ++ violatedClauses.flatMap({case (clause,maxOcc) => clause})
  }

  override def checkInternals(c: Checker) {
    c.check(this.value equals computeFromScratch(s.value) ,Some("included subset Error value=" + this.value  + " should be:" + computeFromScratch(s.value)))
  }
}

