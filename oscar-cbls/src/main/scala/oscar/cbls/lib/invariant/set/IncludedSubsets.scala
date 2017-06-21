package oscar.cbls.lib.invariant.set

import oscar.cbls.algo.quick.QList
import oscar.cbls.core.computation.{ChangingSetValue, SetNotificationTarget, IntInvariant, SetValue}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

/**
 *
 * @param s
 * @param subsetToMonitorAndMaxValues iterable of (subset, max occurrents in the subset, weight in case of violation)
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
    for(deleted <- removedValues) notifyDelete(deleted)
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

