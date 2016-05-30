package oscar.cbls.invariants.lib.logic

import oscar.cbls.invariants.core.computation.{InvariantHelper, _}
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.lib.numeric.Sum

/**
  * Created by gustavbjordal on 27/05/16.
  */
case class BinPackingLoad(items: Array[IntValue], itemsizes: Array[Int]) extends Invariant with IntNotificationTarget{
  for (v <- items.indices) registerStaticAndDynamicDependency(items(v),v)

  finishInitialization()

  private val (minVal,maxVal) = InvariantHelper.getMinMaxBounds(items)
  assert(minVal==0)
  private val maxLoad = itemsizes.foldLeft(0)((acc,l) => acc+ l)

  val binLoad = Array.tabulate(maxVal+1)(i => CBLSIntVar(this.model,0, 0 to maxLoad, "bin_"+i))

  for(i <- items.indices)
    binLoad(items(i).value) :+= itemsizes(i)

  println(binLoad.mkString(", "))

  def Defines(loads:Array[CBLSIntVar]) = {
    assert(loads.length == binLoad.length)
    for(i <- loads.indices)
      loads(i) <== binLoad(i)
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit = {
    binLoad(OldVal) :-= itemsizes(id)
    binLoad(NewVal) :+= itemsizes(id)
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker) {
    c.check(true,Some("nothing to check, invariant is discharged"))
  }


}