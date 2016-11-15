/*******************************************************************************
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
 ******************************************************************************/

package oscar.cp.constraints

import oscar.cp._
import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.ReversibleBoolean
import oscar.algo.SortUtils
import oscar.algo.search.Outcome
import oscar.cp.core.CPPropagStrength

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class UnaryRank(val starts: Array[CPIntVar], val durations: Array[CPIntVar], val ends: Array[CPIntVar]) extends Constraint(starts(0).store) {
  
  val n = starts.length
  
  val ranks = Array.fill(n)(CPIntVar(0 until n)(s))
  
  val rank = new ReversibleInt(s,0)
  val isRanked = new ReversibleBoolean(s,false)
  // for each rank, the index of the activity in that rank
  val rankIndex = Array.fill(n)(new ReversibleInt(s,-1))
  
  
  
  override def setup(l: CPPropagStrength): Outcome = {
    if (s.add(allDifferent(ranks),Strong) == Outcome.Failure)
        return Outcome.Failure
    for(i <- 0 until n) {
      if (ranks(i).isBound) valBindIdx(ranks(i),i)
    }

    for (i <- 0 until n) {
      starts(i).callPropagateWhenBoundsChange(this)
      ends(i).callPropagateWhenBoundsChange(this)
      ranks(i).callValBindIdxWhenBind(this,i)
      ranks(i).callPropagateWhenBind(this)
    }

    if (propagate() == Outcome.Failure) {
      return Outcome.Failure
    }

    return Outcome.Suspend
  }

  
  val index = Array.ofDim[Int](n)
  val endMax = Array.ofDim[Int](n)
  val startMin = Array.ofDim[Int](n)
  
  override def propagate(): Outcome = {
    val cr = rank.value
    //println("propagate:"+cr)
    if (cr == n) {
      isRanked.value = true
      return Outcome.Success;
    }
    
    for (i <- 0 until n) {
      index(i) = i
      endMax(i) = -ends(i).max
    }
    
    SortUtils.mergeSort(index, endMax, 0, n)
    
    for (i <- index) {
      if (ranks(i).hasValue(cr)) {
        // we check if i can come in position cr
        // it can only if the ones with potential 
        // higher ranking can be scheduled afterward
        // let Omega the ones with potential higher ranking
        // startMax = startMax(Omega)
        var startMax = Int.MaxValue
        for (j <- index) {
          if (ranks(j).max >= cr && j != i) {
            startMax = math.min(starts(j).max, startMax-durations(j).min)
          }
        } 
        if (startMax < ends(i).min) {
          if (ranks(i).removeValue(cr) == Outcome.Failure) {
            return Outcome.Failure
          }
        }
      }
    }
    //update starting time
    var minEndTimeInCr = Int.MaxValue
    for (i <- 0 until n)
      if (ranks(i).hasValue(cr))
        minEndTimeInCr = math.min(minEndTimeInCr,ends(i).min);
    for (i <- 0 until n) {
      if (ranks(i).min > cr) {
        // since i cannot come in cr it must be scheduled after
        if (starts(i).updateMin(minEndTimeInCr) == Outcome.Failure)
          return Outcome.Failure;
      }

    }
    return Outcome.Suspend
  }
  
  override def valBindIdx(x: CPIntVar,idx: Int): Outcome = {
    var rankedValue = x.value
    rankIndex(rankedValue).value = idx
    
    
    if (rank.value == rankedValue) {
      // do we have more ranked value consecutive  to this one
      do {
       rankedValue += 1
       rank.value = rankedValue
      } while (ranks.exists { x => x.isBoundTo(rankedValue) })
    }
    
    
    for (i <- 0 until n) {
      index(i) = i
      startMin(i) = starts(i).min
    }
    SortUtils.mergeSort(index, startMin, 0, n)
    
    var minEndTime = Int.MinValue // minimum end time for each activity ranked before current rank
    for (i <- 0 until rank.value) {
      minEndTime = math.max(minEndTime+durations(rankIndex(i)).min,ends(rankIndex(i)).min)
    }
    //println("minEndTime "+minEndTime+" ranked up to "+(rank.value-1))
    //println("ranks:"+ranks.mkString(","))
    for (i <- 0 until n) {
      if (!ranks(i).isBound) {
        //println("=> update min of "+i+" to "+minEndTime)
        if (starts(i).updateMin(minEndTime) == Outcome.Failure) {
          return Outcome.Failure
        }
      }
    }
    //println("starts:"+starts.mkString(","))
    
    
    
    
    return Outcome.Suspend
  }


}
