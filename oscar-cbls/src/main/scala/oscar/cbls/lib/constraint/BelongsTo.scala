package oscar.cbls.lib.constraint

/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

import java.lang.IndexOutOfBoundsException
import java.util

import oscar.cbls._
import oscar.cbls.core._

import scala.collection.immutable.SortedSet
import scala.collection.mutable

/**
 * implements v \in set
 * @author renaud.delandtsheer@cetic.be
 */
case class BelongsTo(v: IntValue, set: SetValue)
  extends Invariant
  with Constraint
  with IntNotificationTarget
  with SetNotificationTarget{

  registerConstrainedVariables(v, set)
  registerStaticAndDynamicDependenciesNoID(v, set)
  finishInitialization()

  /** the violation is 1L if v is not in set, 0L otherwise*/
  override val violation: CBLSIntVar = CBLSIntVar(model, if (set.value.contains(v.value)) 0L else 1L, 0L to 1L, "belongsTo(" + v.name + "," + set.name + ")")

  violation.setDefiningInvariant(this)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long) {
    violation := (if (set.value.contains(v.value)) 0L else 1L)
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Long], removedValues: Iterable[Long], oldValue: SortedSet[Long], newValue: SortedSet[Long]): Unit = {
    if (newValue.contains(this.v.value)) violation := 0L
    else violation := 1L
  }

  /** the violation is 1L v is not is set, 0L otherwise*/
  val constZero:IntValue = 0L
  override def violation(v: Value): IntValue = { if (this.v == v || this.set == v) violation else constZero}

  /**
   * To override whenever possible to spot errors in invariants.
   * this will be called for each invariant after propagation is performed.
   * It requires that the Model is instantiated with the variable debug set to true.
   */
  override def checkInternals(c: Checker) {
    c.check(violation.value == (if (set.value.contains(v.value)) 0L else 1L),
      Some("Violation.value (" + violation.value
        + ") == (if(set.value" + set.value + ".contains(v.value (" + v.value + "))) 0L else 1L)"))
  }
}


/**
  * implements v \in set where set is a constant set
  * Precomputes all possible distances -> constant time updates
  * @author gustav.bjordal@it.uu.se
  */
case class BelongsToConstPreComputing(v: IntValue, set: Set[Long])
    extends Invariant
    with Constraint
    with IntNotificationTarget{

  val sorted = set.toArray[Long].sorted

  def dist(value:Long, s:Set[Long]):Long ={
    val idx = java.util.Arrays.binarySearch(sorted,value)
    if(idx >= 0L){
      0L
    }else{
      val insertionPoint = -(idx+1L)
      if(insertionPoint == 0L){
        math.abs(sorted(insertionPoint)-value)
      }else if(insertionPoint == sorted.length){
        math.abs(sorted(insertionPoint-1L)-value)
      }else {
        math.min(
          math.abs(sorted(insertionPoint)-value),
          math.abs(sorted(insertionPoint-1L)-value))
      }
    }
  }

  val dists = Array.tabulate(v.max-v.min+1L)(i => dist(i+v.min,set))
  val maxDist = dists.max
  registerConstrainedVariables(v)
  registerStaticAndDynamicDependenciesNoID(v)
  finishInitialization()

  /** the violation is 1L if v is not in set, 0L otherwise*/
  override val violation: CBLSIntVar = CBLSIntVar(model, dists(v.value-v.min), 0L to dists.max, "belongsToPreComp(" + v.name + "," + set.mkString("{",",","}") + ")")

  violation.setDefiningInvariant(this)


  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long) {
      try{
        violation := dists(NewVal-v.min)
      }catch{
        case e:IndexOutOfBoundsException =>
          violation := maxDist
      }
  }

  /** the violation is 1L v is not is set, 0L otherwise*/
  override def violation(v: Value): IntValue = { if (this.v == v) violation else 0L }
  /**
    * To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker) {
  }
}

/**
  * implements v \in set where set is a constant set
  * Computes updates on the fly and caches results.
  * @author gustav.bjordal@it.uu.se
  */
case class BelongsToConstCaching(v: IntValue, set: Set[Long])
  extends Invariant
    with Constraint
    with IntNotificationTarget{


  val sorted = set.toArray[Long].sorted

  def dist(value:Long, s:Set[Long]):Long ={
    val idx = java.util.Arrays.binarySearch(sorted,value)
    if(idx >= 0L){
      0L
    }else{
      val insertionPoint = -(idx+1L)
      if(insertionPoint == 0L){
        math.abs(sorted(insertionPoint)-value)
      }else if(insertionPoint == sorted.length){
        math.abs(sorted(insertionPoint-1L)-value)
      }else {
        math.min(
          math.abs(sorted(insertionPoint)-value),
          math.abs(sorted(insertionPoint-1L)-value))
      }
    }
  }

  val dists = mutable.HashMap.empty[Long,Long]
  registerConstrainedVariables(v)
  registerStaticAndDynamicDependenciesNoID(v)
  finishInitialization()

  /** the violation is 1L if v is not in set, 0L otherwise*/
  override val violation: CBLSIntVar = CBLSIntVar(model, dist(v.value-v.min,set), 0L to Math.max(v.max - set.min, set.max - v.min), "belongsToCache(" + v.name + "," + set.mkString("{",",","}") + ")")

  violation.setDefiningInvariant(this)


  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long) {
    if(dists.contains(NewVal)) {
      violation := dists(NewVal)
    }else {
      val d = dist(NewVal, set)
      dists += NewVal -> d
      violation := d
    }
  }

  /** the violation is 1L v is not is set, 0L otherwise*/
  override def violation(v: Value): IntValue = { if (this.v == v) violation else 0L }
  /**
    * To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker) {
  }
}

object BelongsToConst{
  def apply(v: IntValue, set: Set[Long]) = {
    if(v.max - v.min < 1000000L){
      BelongsToConstPreComputing(v,set)
    }else{
      BelongsToConstCaching(v,set)
    }
  }
}