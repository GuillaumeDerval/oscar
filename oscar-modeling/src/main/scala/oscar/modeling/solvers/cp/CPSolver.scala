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

package oscar.modeling.solvers.cp

import oscar.algo.search.{Branching, DFSearch}
import oscar.modeling.misc.SearchStatistics
import oscar.modeling.models.{ModelDeclaration, ModelDeclarationProxy, UninstantiatedModel}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.solvers.SolveHolder
import oscar.modeling.solvers.cp.Branchings.{Alternative, BranchingInstantiator}

import scala.collection.mutable

/**
 * A CPSolver, that can run a local OscaR-CP solver from an uninstantiated model
 */
class CPSolver[RetVal](cpModel: CPModel) extends SolveHolder[RetVal] with CPSearchHolder with ModelDeclarationProxy {
  override implicit val md = cpModel.declaration
  override protected val solveRedirectTo: Any = md

  def getSearch: BranchingInstantiator = getCPSearch
  def setSearch(b: Branching): Unit = setCPSearch(b)
  def setSearch(b: => Seq[Alternative]): Unit = setCPSearch(b)
  def setSearch(b: BranchingInstantiator): Unit = setCPSearch(b)

  def solve(nSols: Int = 0, maxTime: Int = 0): (SearchStatistics, List[RetVal]) = {
    //Build stop condition
    val maxTimestamp = maxTime + System.currentTimeMillis()
    val stopCondition = (s: DFSearch) => {
      var stop = false
      stop |= (nSols != 0 && s.nSolutions >= nSols)
      stop |= (maxTime != 0 && System.currentTimeMillis() >= maxTimestamp)
      stop
    }

    val solutions = mutable.ArrayBuffer[RetVal]()

    assertModel()
    cpModel.cpSolver.searchEngine.onSolution {solutions += onSolution()}
    cpModel.cpSolver.search(getSearch(cpModel))
    val result = cpModel.cpSolver.startSubjectTo(stopCondition, Int.MaxValue, null) {}
    cpModel.cpSolver.searchEngine.clearOnSolution()

    (new SearchStatistics(result), solutions.toList)
  }

  def solveSubjectTo(nSols: Int = 0, maxTime: Int = 0)(func: => Unit): (SearchStatistics, List[RetVal]) = {
    fork {
      func
      solve(nSols, maxTime)
    }
  }

  def use(): CPSolver[RetVal] = {
    md.setCurrentModel(cpModel)
    this
  }

  /**
   * Ensures the current model is cpModel
   */
  private def assertModel(): Unit = {
    if(md.getCurrentModel != cpModel) {
      throw new RuntimeException("Current model is not the one of this CPSolver! Please use cpsolver.use().")
    }
  }
}

object CPSolver {
  def apply[RetVal]()(implicit modelDeclaration: ModelDeclaration): CPSolver[RetVal] = CPSolver(modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel])
  def apply[RetVal](model: UninstantiatedModel): CPSolver[RetVal] = new CPSolver(new CPModel(model))
}