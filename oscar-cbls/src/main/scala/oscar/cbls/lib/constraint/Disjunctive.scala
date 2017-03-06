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
/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/


package oscar.cbls.lib.constraint

import oscar.cbls.core.computation.CBLSIntVar._
import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedMap

/**
 * Implement the Disjunctive constraint.
 * @param start: the start time of each task
 * @param duration: the duration of each task
 * @author Jean-Noël Monette 
 */
case class Disjunctive(start: Array[IntValue],
                      duration: Array[Int]) extends Invariant with Constraint with IntNotificationTarget{
//TODO: Make duration also a var
  
  registerStaticAndDynamicDependencyArrayIndex(start)
  registerConstrainedVariables(start)
  finishInitialization()
  
  private val sumdur = duration.foldLeft(0)((acc,v) => acc + v);
  
  private val Violation: CBLSIntVar = new CBLSIntVar(model, 0, (0 to sumdur*start.length), "ViolationOfDisjunctive")
  Violation.setDefiningInvariant(this)


  //the degree of violation of a task is the sum of the sizes of its overlap with other tasks.
  private val Violations: SortedMap[IntValue, CBLSIntVar] = start.foldLeft(
    SortedMap.empty[IntValue, CBLSIntVar])(
      (acc, intvar) => {
        val newvar = new CBLSIntVar(model, 0, (0 to sumdur), "Violation_Disjunctive_" + intvar.name)
        acc + ((intvar, newvar))
      })
  
  for(i <- 0 until start.length){
    val curstart = start(i).value
    val curduration = duration(i)
    val curend = curstart+curduration
    for(j <- i+1 until start.length){
      val nextstart = start(j).value
      val nextduration = duration(j)
      val nextend = nextstart + nextduration
      val overlap = math.max(0,math.min(math.min(nextend-curstart,curend-nextstart),math.min(curduration,nextduration)))
      Violations(start(i)) :+= overlap
      Violations(start(j)) :+= overlap
      Violation :+= overlap
    }
  }
  
  //oldstarts is used to make sure that only one variable is handled at a time.
  private val oldstarts = start.map(v => v.value)
  
  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, oldstart: Int, newstart: Int) {
    //TODO: This is not completely incremental (but still linear instead of quadratic)!
    val dur = duration(index)
    val oldend = oldstart + dur
    val newend = newstart + dur
    for(j <- 0 until start.length){
      if(j!=index){
        val js = oldstarts(j)
        val jd = duration(j)
        val je = js + jd
        val oldoverlap = math.max(0,math.min(math.min(je-oldstart,oldend-js),math.min(dur,jd)))
        val newoverlap = math.max(0,math.min(math.min(je-newstart,newend-js),math.min(dur,jd)))
        Violations(v) :+= (newoverlap-oldoverlap)
        Violations(start(j)) :+= (newoverlap-oldoverlap)
        Violation :+= (newoverlap-oldoverlap)
      }
    }
    oldstarts(index) = newstart
  }


  override def violation = Violation

  override def violation(v: Value): IntValue = {
    Violations(v.asInstanceOf[IntValue])
  }

  override def checkInternals(c: Checker) {
    //TODO
  }
}


