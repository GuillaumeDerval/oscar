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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.test.invariants

import oscar.cbls.core.computation._
import oscar.cbls.lib.invariant.minmax._
import oscar.cbls.lib.invariant.numeric._
import oscar.cbls.lib.invariant.set._
import oscar.cbls.lib.search.LinearSelectorTrait
import oscar.cbls.modeling.Algebra._

import scala.collection.immutable.SortedSet


object test extends LinearSelectorTrait {

  def main(args: Array[String]) {

    val m: Store = new Store

    val min = 0
    val max = 10000

    val a:CBLSIntVar = CBLSIntVar(m, 9, min to max, "a")
    val b:CBLSIntVar = CBLSIntVar(m, 5, min to max, "b")
    val c:CBLSIntVar = CBLSIntVar(m, 6, min to max, "c")
    val d:CBLSIntVar = CBLSIntVar(m, 6, min to max, "d")
    val e:CBLSSetVar = new CBLSSetVar(m, SortedSet.empty[Int], min to max, "e")

    d <== (5 + c + 5 + (b * (4 - 3)))
    c <== a + b //Sum(SortedSet(a, b))
    e <== Inter(MakeSet(SortedSet(a, b)), MakeSet(SortedSet(b, c)))
    val Const5 = CBLSIntConst(5)
    val f = MaxArray(SortedSet(a,b,c,d).toArray) + MinLin(SortedSet(Abs(a),b,Const5,d))
    val g = MaxLin(SortedSet(a,b,c,d))

    val h = ArgMin(Array(a,d,b,d,c,d - 1))
  
//    Event(h,{println("Trigger: h changed: " + h)})
    val k = Cardinality(h)
//    Event(k,{println("Trigger: k changed: " + k)})

    Event(c,{println("Trigger: c changed: " + c)})

    //TriggerOn(cstr.violation(d), {println("Violation change on d: " + e)})

    m.close()
    println("closed")

    for(j <- Range(1,100,1)){a := j; println(h)}
    a := 5
    println("just changed a to 5")
    println("" + c + " " + d)
    b := 2
    println("just changed b to 2")
    println("" + c + " " + d)
    m.propagate()
    
    //while(cstr.Violation != 0){
    //  val tochange:IntVar = selectMax(List(a,b),v => cstr.violation(v))
    //  val newval = selectMin(tochange.getDomain(), i => cstr.GetAssignDelta(tochange,i))
    //  tochange := newval
    //}

    println("" + c + " " + d)

    println(m.solution(false))

  }
}
