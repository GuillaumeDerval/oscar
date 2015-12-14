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
  *         by Gaël Thouvenin
  ******************************************************************************/

package oscar.examples.cbls

import oscar.cbls.constraints.lib.basic.EQ
import oscar.cbls.invariants.core.computation.{FullRange, CBLSIntVar}
import oscar.cbls.invariants.lib.logic.SelectLESetQueue
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.modeling.{CBLSModel, AlgebraTrait}
import oscar.cbls.constraints.lib.global.Exactly
import oscar.cbls.search.StopWatch

import scala.collection.immutable.SortedMap

object MagicSeries extends CBLSModel with App with AlgebraTrait with StopWatch{

  //The size of the serie
  val size:Int = 7

  val range: Range = Range(0, size)

  val magicSeries = Array.tabulate(size)(w => CBLSIntVar(0, 0 until size, " n° of " + w ))

  //exactly constraint
  val bounds = SortedMap[Int, CBLSIntVar]((for(v <- range) yield v -> magicSeries(v)):_*)
  c.post(Exactly(magicSeries, bounds))

  //redundant constraint, to make the search procedure faster
  c.post(EQ(Sum(for(i <- range) yield (i * magicSeries(i))), size))

  var it: CBLSIntVar = CBLSIntVar(0, FullRange, "it")

  val tabuArray: Array[CBLSIntVar] = Array.tabulate(size)(w => CBLSIntVar(-1, FullRange, "tabu_of_" + w + " "))

  val nonTabuPositions = SelectLESetQueue(tabuArray, it).setName("non tabu positions")

  val maxs = argMax(Array.tabulate(size)(i => c.violation(magicSeries(i))), nonTabuPositions)

  val maxVal = maxs.setName("most violated positions")

  close()

  val tabuLength = 3

  val restartFreq = 300

  startWatch()

  //Search Procedure
  while(violation.value > 0)
  {
    var index = maxVal.value.firstKey

    var bestOccurrences = selectMin(range)(r => assignVal(magicSeries(index), r), r => magicSeries(index).value != r)

    magicSeries(index) := bestOccurrences
    tabuArray(index) := it.value + tabuLength
    it :+= 1
  }

  print("Solution found : \n(")
  for(i <- range) yield print(magicSeries(i).value + ", ")
  print(")\nin " + it.value + " iterations and " + getWatch + " ms")
}
