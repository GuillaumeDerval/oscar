package oscar.cbls.lib.invariant.set

import oscar.cbls.algo.quick.QList
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

/**
 * sum(on s:subsetToMonitorAndMaxValues if #(s._1 inter s) > s._2 of s._3 )
 * @param s a setValue
 * @param subsetToMonitorAndMaxValues iterable of (subset, max occurrence in the subset, weight in case of violation)
 */
case class IncludedSubsets(s: SetValue, subsetToMonitorAndMaxValues:Iterable[(Iterable[Int],Int,Int)])
  extends IntInvariant(0,0 to subsetToMonitorAndMaxValues.size)
  with SetNotificationTarget{

  registerStaticAndDynamicDependenciesNoID(s)
  finishInitialization()

  require(s.min == 0)

  val subsetAndMaxAndWeightArray = subsetToMonitorAndMaxValues.toArray
  val n = subsetAndMaxAndWeightArray.length

  val valueToSubsetID:Array[QList[Int]] = Array.fill(s.max+1)(null)

  for (forbiddenID <- subsetAndMaxAndWeightArray.indices) {
    val (values,maxNumber,weight) = subsetAndMaxAndWeightArray(forbiddenID)
    for(value <- values){
      valueToSubsetID(value) = QList(forbiddenID,valueToSubsetID(value))
    }
  }

  this := 0
  val subsetToNbPresent:Array[Int] = Array.fill(n)(0)
  for(value <- s.value){
    notifyInsert(value)
  }

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int],
                                removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    for (added <- addedValues) notifyInsert(added)
    for (deleted <- removedValues) notifyDelete(deleted)
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
  private def notifyDelete(value: Int) {
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


/**
 * @param s
 * @param subsetToMonitorAndMaxOcc iterable of (subset, max occurrence in the subset)
 */
case class PossibleExtensions(s: SetValue, allVal:SortedSet[Int], subsetToMonitorAndMaxOcc:Iterable[(Iterable[Int],Int)])
  extends SetInvariant(initialDomain = Domain(allVal))
  with SetNotificationTarget{

  registerStaticAndDynamicDependenciesNoID(s)
  finishInitialization()

  require(s.min == 0)

  val subsetAndMax = subsetToMonitorAndMaxOcc.toArray
  val n = subsetAndMax.length

  //building valueToClauseIDs
  val valueToClauseIDs:Array[QList[Int]] = Array.fill(s.max+1)(null)
  for (clauseID <- subsetAndMax.indices) {
    val (values,maxNumber,weight) = subsetAndMax(clauseID)
    for(value <- values){
      valueToClauseIDs(value) = QList(clauseID,valueToClauseIDs(value))
    }
  }

  //value to number of violated clauses this is in
val valToNumberOfViolatedClauses:Array[Int] = Array.fill(n)(0)

  //clause to number of values in it


  //init
  this := allVal
  val subsetToNbPresent:Array[Int] = Array.fill(n)(0)
  for(value <- s.value){
    notifyInsert(value)
  }

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int],
                                removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    for (added <- addedValues) notifyInsert(added)
    for (deleted <- removedValues) notifyDelete(deleted)
  }

  @inline
  private def notifyInsert(value: Int) {
    for(subset <- QList.toIterable(valueToClauseIDs(value))){
      subsetToNbPresent(subset) = subsetToNbPresent(subset) + 1
      if(subsetToNbPresent(subset) == subsetAndMaxAndWeightArray(subset)._2 + 1) {
        this :+= subsetAndMaxAndWeightArray(subset)._3
      }
    }
  }

  @inline
  private def notifyDelete(value: Int) {
    for(subset <- QList.toIterable(valueToClauseIDs(value))){
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

