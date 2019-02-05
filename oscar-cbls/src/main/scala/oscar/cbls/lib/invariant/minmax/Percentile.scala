package oscar.cbls.lib.invariant.minmax

import oscar.cbls.IntValue
import oscar.cbls.algo.search.LazyQuicksort
import oscar.cbls.core._
import oscar.cbls.core.computation.Domain


class Percentile(vars: Array[IntValue], nTh: Int, smallest:Boolean = false)
  extends IntInvariant(initialDomain = Domain(vars.foldLeft((Long.MaxValue,Long.MinValue))((acc, v) => ((acc._1 min v.min),acc._2 max v.max))),
    initialValue = vars.foldLeft(Long.MaxValue)((acc, v) => acc min v.min))
    with IntNotificationTarget{

  require(nTh >= 0, "nthShould be positive")
  require(nTh < vars.length, "nTh should fall within the number of variables")

  require(nTh <= (vars.length/2), "invert the invariant for efficiency reasons, nTh:" + nTh + " should be <="+(vars.length/2))

  registerStaticAndDynamicDependencyArrayIndex(vars)
  finishInitialization()

  //if MaxInt, then this is stable
  var changeTrackSinceLastPropagation:Int = Int.MaxValue

  scheduleForPropagation()

  //the principle is that we keep output unchanged unless ome value that on onee side of the percentile changes side.
  //we count the number of such changes (positive if goes aboe, negative if goes below)
  //exception if the changed value is the percentile
  //upon propagation, if the amount is zero, nothing to do, otherwise, we compute the percentile from scratch, unsing lazy quicksort method.
  //the signal itself is setting the counter to MaxInt
  override def notifyIntChanged(v: ChangingIntValue, id: Int, oldVal: Long, newVal: Long): Unit = {
    if (changeTrackSinceLastPropagation == Int.MaxValue) return

    val currentValue = this.newValue

    if (oldVal == currentValue){
      changeTrackSinceLastPropagation = Int.MaxValue
      scheduleForPropagation()
    }else if(oldVal < currentValue && currentValue < newVal){
      changeTrackSinceLastPropagation += 1
      scheduleForPropagation()
    }else if(currentValue  < oldVal && newVal < currentValue ){
      changeTrackSinceLastPropagation -= 1
      scheduleForPropagation()
    }//So we do not even schedule for propagation if a change does not impact percentile
  }

  override def performInvariantPropagation(): Unit = {
    if(changeTrackSinceLastPropagation == 0) return //change are compensating each other
    changeTrackSinceLastPropagation = 0

    //we use a lazy quicksort here.
    val lq = new LazyQuicksort(vars.map(_.value), if (smallest) (a => a) else (a => -a))

    this := lq(nTh)
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {

    val sortedValues:Array[Long] = this.vars.map(_.value).sorted
    if(smallest){
      c.check(sortedValues(nTh) == this.newValue,Some("smallest sortedValues:" +sortedValues.mkString(",") + " nTh:" + nTh + " expected:" +this.newValue))
    }else{
      c.check(sortedValues(vars.length - (nTh + 1)) == this.newValue,Some("biggest sortedValues:" +sortedValues.mkString(",") + " nTh:" + nTh + " expected:" +this.newValue))
    }
  }
}