package oscar.cbls.lib.invariant

import oscar.cbls.algo.heap.{ArrayMap, BinomialHeapWithMoveExtMem}
import oscar.cbls.algo.quick.QList
import oscar.cbls.core.computation._

import scala.collection.immutable.SortedSet




/**
 * Maintains Miax(Var(i) | i in cond)
 * Exact ordering is specified by implementing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, cannot be null
 * update is O(log(n)), but probably faster if you do neighborhood exploration with moves and backtracks
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class MiaxConstArrayLazy2(vars: Array[Int], cond: Array[ChangingIntValue], default: Int, maxBacklog:Int = Int.MaxValue)
  extends IntInvariant
  with SetNotificationTarget{

  //  var nbAnihilation = 0
  //  var nbDoIt = 0

  val n = vars.length
  var h: BinomialHeapWithMoveExtMem[Int] = new BinomialHeapWithMoveExtMem[Int](i => Ord(vars(i)), vars.length, new ArrayMap(vars.length))

  var backLog:QList[Int] = null
  var backlogSize:Int = 0
  val isBacklogged:Array[Boolean] = Array.fill(vars.size)(false)
  val consideredValue:Array[Boolean] = Array.fill(vars.size)(false)

  registerStaticAndDynamicDependency(cond)
  finishInitialization()

  def computeMinMax():(Int,Int) = {
    var myMin = default
    var myMax = default
    for (i <- vars) {
      if(i > myMax) myMax = i
      if(i < myMin) myMin = i
    }
    (myMin,myMax)
  }

  restrictDomain(computeMinMax())

  for (i <- cond.value) {
    h.insert(i)
    consideredValue(i) = true
  }

  def Ord(v: Int): Int

  @inline
  private[this] def updateFromHeap() {
    if (h.isEmpty) {
      this := default
    } else {
      updateFromNonEmptyHeap()
    }
  }

  @inline
  private[this] def updateFromNonEmptyHeap() {
    this := vars(h.getFirst)
  }

  updateFromHeap()

  @inline
  def equalOrNotImpactingMiax(potentialMiax:Int):Boolean

  @inline
  private[this] def putIntoBackLog(cond:Int): Unit ={
    if(!isBacklogged(cond)){
      backLog = QList(cond,backLog)
      isBacklogged(cond) = true
      backlogSize += 1
    }
  }

  /**
   * does not perform final update because was not supposed to be impacted
   */
  @inline
  private[this] def trimBackLog(){
    while(true){
      if(backLog == null) return
      if(!isBacklogged(backLog.head)){
        backlogSize -=1
        backLog = backLog.tail
      } else if(backlogSize > maxBacklog){
        val condValue = backLog.head
        backlogSize -=1
        backLog = backLog.tail
        processThisRealBackLog(condValue)
      }else{
        return
      }
    }
  }

  @inline
  private[this] def processThisRealBackLog(condValue:Int): Unit ={
    if(consideredValue(condValue)){ //should be removed
      assert(!cond.value.contains(condValue))
      h.delete(condValue)
      consideredValue(condValue) = false
    }else{ //should be added
      assert(cond.value.contains(condValue))
      h.insert(condValue)
      consideredValue(condValue) = true
    }
    isBacklogged(condValue) = false
  }

  @inline
  private[this] def processBackLog(): Unit ={
    while(backLog != null){
      val condValue = backLog.head
      backLog = backLog.tail
      if(isBacklogged(condValue)) {
        processThisRealBackLog(condValue)
      }
    }
    backlogSize = 0
  }

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    //insert first because reduces chances of flush
    val itAdded = addedValues.iterator
    while(itAdded.hasNext){
      notifyInsertOn(v: ChangingSetValue, itAdded.next())
    }

    val itDeleted = removedValues.iterator
    while(itDeleted.hasNext){
      notifyDeleteOn(v: ChangingSetValue, itDeleted.next())
    }
  }

  @inline
  def notifyAddedValue(value: Int) {
    if(consideredValue(value)){ //anihilation
      assert(isBacklogged(value))
      isBacklogged(value) = false
      return
    }
    if(equalOrNotImpactingMiax(vars(value))){//backlog
      trimBackLog()
      putIntoBackLog(value)
    }else{//impacted
      this := vars(value)
      h.insert(value)
      consideredValue(value) = true
    }
  }

  @inline
  def notifyRemovedValue(value: Int) {
    if(!consideredValue(value)){ //anihilation
      assert(isBacklogged(value))
      isBacklogged(value) = false
      return
    }
    if(this.newValue == vars(value)){//impacted, flush backLog
      processBackLog()
      h.delete(value)
      consideredValue(value) = false
      updateFromHeap()
    }else{//not impacted, backlog
      trimBackLog()
      putIntoBackLog(value)
    }
  }
}
