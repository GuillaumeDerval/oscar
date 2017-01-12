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

package oscar.examples.cbls

import oscar.cbls.business.binPacking.model.BinPackingProblem
import oscar.cbls.business.binPacking.solver.{EmptyMostViolatedBin, JumpSwapItems, SwapItems, MoveItem}
import oscar.cbls.modeling.CBLSModel

object BinPackingAndCombinators extends CBLSModel with App{

  //the index is the first element of the couple
  def indexList(l:List[Int]):List[(Int,Int)] = null

  val itemSizes = List(20, 5, 7, 6, 3, 5, 9, 7, 3, 5)

  val binSizes = List(20, 21, 29)

  val problem = BinPackingProblem(itemSizes,binSizes,s, c, 0)

  s.close()

  val x =  ((MoveItem(problem) exhaustBack SwapItems(problem))
    exhaust (MoveItem(problem, true) exhaustBack SwapItems(problem, true)
    orElse (JumpSwapItems(problem) maxMoves 3 beforeMove println("Jump"))
    orElse (EmptyMostViolatedBin(problem) beforeMove println("BigJump")))) saveBest problem.overallViolation

  x.verbose = 1
  x.doAllMoves(_ >= 200 || problem.overallViolation.value == 0, problem.overallViolation)

  x.restoreBest()

  println(problem)
}
