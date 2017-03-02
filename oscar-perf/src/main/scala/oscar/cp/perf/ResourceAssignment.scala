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

package oscar.cp.perf


import oscar.cp._

/**
 * Problem statement :
 * Problem coming from
 * http://www.emn.fr/z-info/sdemasse/gccat/Kassignment_to_the_same_set_of_values.html#uid3286
 * 
 * Each task, is divided into subtasks and each subtask has an associated weight.
 * 
 * 
 * We consider 9 bins, each with capacity 5 that are partitioned disjoint groups of bins
 * {2,3,7} , {0,4}, {5,6}, and {1,8}.
 * 
 * The objective is to assign the substasks to the bins enforcing the fact that all subtasks 
 * that are associated with the same task are assigned the same group of bins. 
 * In addition, the maximum sum of the weights of the subtasks that are assigned the same bin 
 * should be minimized
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object ResourceAssignment extends CPModel with App {

  solver.silent = true
  val binCapa = 20
  val partition = Array(Set(2, 3, 7), Set(0, 4), Set(5, 6), Set(1, 8))

  val taskWeight = Array((0, 3), (0, 3), (0, 3), (0, 2), (0, 2), (0, 2),
    (1, 3), (1, 3), (1, 2), (1, 2),
    (2, 2), (2, 2), (2, 3), (2, 2), (2, 1),
    (3, 3), (3, 2), (3, 2), (3, 2), (3, 1),
    (4, 3), (4, 2), (4, 1)) // for each subtask: (super task, weight)
    
  val nbBins = partition.flatten.max + 1
  val nbTasks = taskWeight.map(_._1).max + 1

  // p(t) is the partition chosen for task i
  val p = Array.fill(nbTasks)(CPIntVar(0 until partition.size))

  // x(i) is the bin chosen for subtask i
  val x: Array[CPIntVar] = for ((i, j) <- taskWeight) yield {
    val possbin = Array.tabulate(partition.size)(i => CPIntVar(partition(i)))
    val xij = CPIntVar(0 until nbBins)
    possbin(p(i))
  }

  val load = Array.fill(nbBins)(CPIntVar(0 to binCapa))

  add(maximum((0 until nbBins).map(load(_))) <= 6)
  add(binPacking(x, taskWeight.map(_._2), load))

  // some stupid constraints to make it tractable
  for(i <- 1 until 9) {
    add(x(i-1) <= x(i))
  }
  add(gcc(x,0 to 8,2,3))
  
  search { binaryFirstFail(x, _.min) }
  println(start())
}
