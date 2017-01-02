package oscar.cp.perf

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

import oscar.cp._

object LargeSort extends CPModel with App {
  val n = 100
  val domains = Array(0 to 100,300 to 400, 500 to 600)
  val x = Array.tabulate(n)(i => CPIntVar(domains(i% domains.size)))
  val s = Array.tabulate(n)(i => CPIntVar(0 to 1000))
  val p = Array.tabulate(n)(i => CPIntVar(0 until n))
  
  solver.post(sortedness(x,s,p,true),Strong)
  
  search {binaryFirstFail(x, _.min)}
  

  //def sortedness(x: IndexedSeq[CPIntVar], s: IndexedSeq[CPIntVar], p: IndexedSeq[CPIntVar], strictly: Boolean = false): LinkedList[Constraint] = {

  println(start(nSols=1))
}
