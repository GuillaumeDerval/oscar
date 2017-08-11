package oscar.cbls.lib.invariant.minmax

import oscar.cbls.algo.heap.{ArrayMap, BinomialHeapWithMoveExtMem, BinomialHeapWithMove}
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

/**
 * @param constArray
 * @param condSet
 * @param default
 * @param maxDiameter is the maximal number of values in condSet that are monitored in the set, must be >=1.
 *                    the actual diameter is kept between 1 and tis value, lazily
 */
class MinConstArrayValueWise(constArray: Array[Int], condSet: SetValue, default: Int,maxDiameter:Int = 2)
  extends IntInvariant with SetNotificationTarget{

  val n = constArray.length
  val range = 0 until n
  val idSortedByIncreasingValue:Array[Int] = range.toArray.sortBy(a => constArray(a))
  var nbListenedVals = 0
  val idToTheirPositionNumber:Array[Int] = Array.fill(n)(-1)
  for(i <- range){
    idToTheirPositionNumber(idSortedByIncreasingValue(i)) = i
  }

  registerStaticDependency(condSet)
  val key:ValueWiseKey = registerDynamicValueWiseDependency(condSet)
  finishInitialization()

  //only the smallest position in cond
  //we listen to these ones (and to the no-included smaller ones as well)
  val heapOfConsideredPositions:BinomialHeapWithMoveExtMem[Int] = new BinomialHeapWithMoveExtMem[Int](i => constArray(i), n, new ArrayMap(n))

  //GetKey:T => Int,val maxsize:Int, position:scala.collection.mutable.Map[T,Int])(implicit val A:Ordering[T],implicit val X:Manifest[T]){
  addValuesIntoListenedUntilDiameterIsStrictlyBiggerThanZeroOrFullScope(condSet.value)

  this := (if(heapOfConsideredPositions.isEmpty) default else constArray(heapOfConsideredPositions.getFirst))

  override def notifySetChanges(v : ChangingSetValue, d : Int,
                                addedValues : Iterable[Int],
                                removedValues : Iterable[Int],
                                oldValue : SortedSet[Int],
                                newValue : SortedSet[Int]){
    //we start wit the added because we want to avoid exploring the values for nothing
    require(d == Int.MinValue)

    for (added <- addedValues) {
      if(idToTheirPositionNumber(added) < nbListenedVals) {
        heapOfConsideredPositions.insert(added)
      }
    }
    for (deleted <- removedValues) {
      if(idToTheirPositionNumber(deleted) < nbListenedVals) {
        heapOfConsideredPositions.delete(deleted)
      }
    }

    val diameter = heapOfConsideredPositions.size
    if(diameter == 0){
      //increase diameter to 1
      addValuesIntoListenedUntilDiameterIsStrictlyBiggerThanZeroOrFullScope(newValue)
    }else if (diameter > maxDiameter){
      //reduce diameter
      removeValuesFromListenedUntilDiameterSmallerOrEqualToMaxDiameter()
      //trimNotListenedTail()
    }

    //update this value
    this := (if(heapOfConsideredPositions.isEmpty) default else constArray(heapOfConsideredPositions.getFirst))
  }

  /**
   * adds one more value into the scope,
   * does not add into the heapOfConsideredPositions
   * @return the iD added into the scope
   */
  def addOneMoveValueIntoScope():Int = {
    val IdToAddIntoTheScope = idSortedByIncreasingValue(nbListenedVals)
    nbListenedVals += 1
    key.addToKey(IdToAddIntoTheScope)
    IdToAddIntoTheScope
  }

  def removeValuesFromListenedUntilDiameterSmallerOrEqualToMaxDiameter(){
    while(heapOfConsideredPositions.size > maxDiameter) {
      require(nbListenedVals >0)
      val removedValue = idSortedByIncreasingValue(nbListenedVals-1)
      nbListenedVals -= 1
      key.removeFromKey(removedValue)
      heapOfConsideredPositions.deleteIfPresent(removedValue)
    }
  }

  def trimNotListenedTail(){
    while(true) {
      val removedValue = idSortedByIncreasingValue(nbListenedVals-1)
      if(heapOfConsideredPositions.contains(removedValue)) return
      nbListenedVals -= 1
      key.removeFromKey(removedValue)
      heapOfConsideredPositions.deleteIfPresent(removedValue)
    }
  }

  def addValuesIntoListenedUntilDiameterIsStrictlyBiggerThanZeroOrFullScope(condValue:SortedSet[Int]){
    while(heapOfConsideredPositions.size == 0 && nbListenedVals < n) {
      val addedValue = addOneMoveValueIntoScope()
      if(condValue contains addedValue) {
        heapOfConsideredPositions.insert(addedValue)
      }
    }
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c : Checker) {
    if (condSet.value.isEmpty) c.check(value == default)
    else c.check(value == constArray(condSet.value.minBy(constArray(_))))
    c.check(nbListenedVals >= heapOfConsideredPositions.size)
  }
}

