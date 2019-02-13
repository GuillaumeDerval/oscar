package oscar.cbls.lib.invariant.minmax

import oscar.cbls.algo.heap.BinomialHeapWithMoveInt
import oscar.cbls._
import oscar.cbls.core._

import scala.collection.immutable.SortedSet

/**
 *
 * @param constArray
 * @param condSet
 * @param default
 * @param maxDiameter is the maximal number of values in condSet that are monitored in the set, must be >=1.
 *                    the actual diameter is kept between 1L and tis value, lazily
 */
class MinConstArrayValueWise(constArray: Array[Long], condSet: SetValue, default: Long, maxDiameter:Long = 2L)
  extends IntInvariant(initialDomain = Domain(constArray.min min default,constArray.max max default))
    with SetNotificationTarget{

  private val n = constArray.length
  private val range = 0L until n

  //TODO: this is very slow!
  private val idSortedByIncreasingValue:Array[Long] = range.toArray.sortBy(a => constArray(a))

  private var nbListenedVals = 0L
  private val idToTheirPositionNumber:Array[Long] = Array.fill(n)(-1L)
  for(i <- range){
    idToTheirPositionNumber(idSortedByIncreasingValue(i)) = i
  }

  //TODO: should be total registration in case there are less than, say, 10L values in the set!

  registerStaticDependency(condSet)
  private val key:ValueWiseKey = registerDynamicValueWiseDependency(condSet)
  finishInitialization()

  //only the smallest position in cond
  //we listen to these ones (and to the no-included smaller ones as well)
  private val heapOfConsideredPositions = new BinomialHeapWithMoveInt(i => constArray(i),n, n)

  //GetKey:T => Long,val maxsize:Long, position:scala.collection.mutable.Map[T,Long])(implicit val A:Ordering[T],implicit val X:Manifest[T]){
  addValuesIntoListenedUntilDiameterIsStrictlyBiggerThanZeroOrFullScope(condSet.value)

  this := (if(heapOfConsideredPositions.isEmpty) default else constArray(heapOfConsideredPositions.getFirst))

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Long], removedValues: Iterable[Long], oldValue: SortedSet[Long], newValue: SortedSet[Long]): Unit = {
    //we start wit the added because we want to avoid exploring the values for nothing
    require(id == Int.MinValue)

    val removedValuesIt = removedValues.iterator
    while(removedValuesIt.hasNext){
      val deleted = removedValuesIt.next()
      if(idToTheirPositionNumber(deleted) < nbListenedVals) {
        heapOfConsideredPositions.delete(deleted)
      }
    }

    val addedValuesIt = addedValues.iterator
    while(addedValuesIt.hasNext){
      val added = addedValuesIt.next()
      if(idToTheirPositionNumber(added) < nbListenedVals) {
        heapOfConsideredPositions.insert(added)
      }
    }

    val diameter = heapOfConsideredPositions.size
    if(diameter == 0L){
      //increase diameter to 1L
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
  private def addOneMoveValueIntoScope():Long = {
    val IdToAddIntoTheScope = idSortedByIncreasingValue(nbListenedVals)
    nbListenedVals += 1L
    key.addToKey(IdToAddIntoTheScope)
    IdToAddIntoTheScope
  }

  private def removeValuesFromListenedUntilDiameterSmallerOrEqualToMaxDiameter(){
    while(heapOfConsideredPositions.size > maxDiameter) {
      require(nbListenedVals >0L)
      val removedValue = idSortedByIncreasingValue(nbListenedVals-1L)
      nbListenedVals -= 1L
      key.removeFromKey(removedValue)
      heapOfConsideredPositions.deleteIfPresent(removedValue)
    }
  }

  private def trimNotListenedTail(){
    while(true) {
      val removedValue = idSortedByIncreasingValue(nbListenedVals-1L)
      if(heapOfConsideredPositions.contains(removedValue)) return
      nbListenedVals -= 1L
      key.removeFromKey(removedValue)
      heapOfConsideredPositions.deleteIfPresent(removedValue)
    }
  }

  private def addValuesIntoListenedUntilDiameterIsStrictlyBiggerThanZeroOrFullScope(condValue:SortedSet[Long]){
    while(heapOfConsideredPositions.size == 0L && nbListenedVals < n) {
      val addedValue = addOneMoveValueIntoScope()
      if(condValue contains addedValue) {
        heapOfConsideredPositions.insert(addedValue)
      }
    }
  }

  override def checkInternals(c : Checker) {
    if (condSet.value.isEmpty) c.check(value == default)
    else c.check(value == constArray(condSet.value.minBy(constArray(_))))
    c.check(nbListenedVals >= heapOfConsideredPositions.size)
  }
}

