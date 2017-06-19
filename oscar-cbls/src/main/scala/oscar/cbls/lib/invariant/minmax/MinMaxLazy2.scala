package oscar.cbls.lib.invariant.minmax

import oscar.cbls.algo.dll.DPFDLLStorageElement
import oscar.cbls.algo.heap.{BinomialHeapWithMove, ArrayMap, BinomialHeapWithMoveExtMem}
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.{PropagationElement, KeyForElementRemoval}

import scala.collection.immutable.SortedSet


abstract class MinConstArrayLazy2(constArray: Array[Int], condSet: SetValue, default: Int)(diameter:Int = constArray.length)
  extends IntInvariant
  with SetNotificationTarget{

  val n = constArray.length
  val positionsSortedByIncreasingValue:Array[Int] = constArray.sortBy(a => a)

  registerStaticDependency(condSet)
  val key:ValueWiseKey = registerDynamicValueWiseDependency(condSet)
  finishInitialization()

  //only the smallest position in cond
  //we listen to these ones (and to the no-included smaller ones as well)
  var consideredPositions:BinomialHeapWithMove[Int] = new BinomialHeapWithMove[Int](i => constArray(i), n)

  val cond:Array[Boolean]

  def addToListened(value:Int,addToHeap:Boolean){
    key.addToKey(value)
    if(addToHeap){
      consideredPositions.insert(value)
    }
  }

  def removeFromListened(value:Int,wasTrue:Boolean){
    if(wasTrue){
      consideredPositions.delete(value)
    }

    key.removeFromKey(value)
  }


  override def notifySetChanges(v : ChangingSetValue, d : Int,
                                addedValues : Iterable[Int],
                                removedValues : Iterable[Int],
                                oldValue : SortedSet[Int],
                                newValue : SortedSet[Int]){
    //we start wit the added because they will reduce the diameter
    for (added <- addedValues) notifyAddToCond(added)
    for (deleted <- removedValues) notifyRemoveFromCond(deleted)
    adjustDiameter()
    //update this value
    this := constArray(consideredPositions.getFirst)
  }

  def notifyAddToCond(value:Int){
    consideredPositions.insert(value)

  }

  def notifyRemoveFromCond(value:Int){

  }

  @inline
  def nbListenedVals = consideredPositions.size

  def adjustDiameter(){

  }

  /**
   * increments the number of listened values
   * @return the added indice
   */
  def incrementDiameter(cond:SortedSet[Int]):Int = {
    val addedIndice = positionsSortedByIncreasingValue(nbListenedVals)
    if(cond.contains(addedIndice)) {
      consideredPositions.insert(addedIndice)
    }
    key.addToKey(addedIndice)
    addedIndice
  }

  def decrementDiameter(cond:SortedSet[Int]):Int = {
    val removedIndice = positionsSortedByIncreasingValue(nbListenedVals-1)
    if(consideredPositions contains removedIndice){
      consideredPositions.delete(removedIndice)
    }
    key.removeFromKey(removedIndice)
    removedIndice
  }

  def extendDiameter{

  }

}

