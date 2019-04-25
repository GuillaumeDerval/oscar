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

package oscar.cbls.lib.invariant.logic

import oscar.cbls._
import oscar.cbls.core._
import oscar.cbls.core.computation.{CBLSIntVar, DomainRange, IntValue}

/**
 * Author: Jean-Noël Monette
 */
case class SparseCount(values: Array[IntValue], counts: Map[Long,CBLSIntVar])
  extends Invariant
  with IntNotificationTarget{

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v), v)
  
  finishInitialization()
  for (count <- counts.values) { count := 0L }
  for (v <- values.indices){
    counts.get(values(v).value).foreach(c => c :+= 1L)
  }
  for(c <- counts.values) c.setDefiningInvariant(this)
    
   @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long) {
    assert(values(index) == v)
    counts.get(OldVal).foreach(c => c :-= 1L)
    counts.get(NewVal).foreach(c => c :+= 1L)
  }
}

/**
 * Maintains a count of the indexes of array: count(j) = #{i in index of values | values[i]+offset == j}
 * This is considered as a dense count because counts is an array and must cover all the possibles values of the values in the array ''values''
 *
 * it is expected that the values are always >= 0L
 * @author renaud.delandtsheer@cetic.be
 * */
case class DenseCount(values: Array[IntValue], counts: Array[CBLSIntVar], offset:Long = 0L)
  extends Invariant
  with IntNotificationTarget{

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v), v)

  for (count <- counts) { count := 0L }

  for (v <- values.indices) {
    counts(values(v).value+offset) :+= 1L
  }

  finishInitialization()

  for (c <- counts) { c.setDefiningInvariant(this) }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long) {
    assert(values(index) == v)
    counts(OldVal + offset) :-= 1L
    counts(NewVal + offset) :+= 1L
  }

  override def checkInternals(c: Checker) {
    /**
     * Maintains a count of the indexes of array:
     * count(j) = #{i in index of values | values[i] == j}
     * This is considered as a dense count because counts is
     * an array and must cover all the possibles values of the values
     * in the array ''values''
     */
    val myCounts = Array.fill[Long](counts.length)(0L)
    for (i <- values.indices) {
      val v = values(i).value
      myCounts(v+offset) = myCounts(v+offset) + 1L
    }

    for (j <- counts.indices) {
      c.check(counts(j+offset).value == myCounts(j+offset),
        Some("counts(" + j + "+offset).getValue(false) (" + counts(j+offset).value
          + ") == myCounts(" + j + "+offset) (" + myCounts(j+offset) + ")"))
    }
  }
}

/**
  * Maintains the number of occurances of c \in values
  *
  * @author gustav.bjordal@it.uu.se
  * */

case class ConstCount(values: Array[IntValue], c: Long)
  extends IntInvariant(values.count(v => v.value == c), DomainRange(0L,values.length))
    with IntNotificationTarget{

  registerStaticAndDynamicDependencyArrayIndex(values)

  finishInitialization()

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long) {
    if(NewVal == c){
      this :+= 1L
    }else if(OldVal == c){
      this :-= 1L
    }
  }

  override def checkInternals(c: Checker) {
  }
}


object DenseCount{
  def makeDenseCount(vars: Array[IntValue]):DenseCount = {
    val ((minMin,maxMax)) = InvariantHelper.getMinMaxBounds(vars)
    val mbValues = maxMax - minMin + 1L
    val m:Store = InvariantHelper.findModel(vars)
    val nbVars = vars.length
    val counts = Array.tabulate(mbValues)(i => CBLSIntVar(m,0L, Domain(0L , nbVars), "count_" + (i-minMin)))
    DenseCount(vars,counts,-minMin)
  }
}
