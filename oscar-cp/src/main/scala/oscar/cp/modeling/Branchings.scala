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

package oscar.cp.modeling

import oscar.algo.search.Branching
import oscar.algo.search.BranchingUtils
import oscar.cp.scheduling.search.SetTimesBranching
import oscar.cp.scheduling.search.RankBranching
import oscar.cp.searches._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPSetVar
import oscar.algo.reversible.ReversibleContext

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
trait Branchings extends BranchingUtils {

  def binaryIdx(variables: Array[CPIntVar], varHeuristic: (Int => Int), valHeuristic: (Int => Int)): Branching = {
    new BinaryBranching(variables, varHeuristic, valHeuristic)
  }

  def binaryIdx(variables: Array[CPIntVar], varHeuristic: (Int => Int)): Branching = {
    binaryIdx(variables, varHeuristic, variables(_).min)
  }

  def binary(variables: Array[CPIntVar]): Branching = {
    binaryIdx(variables, variables(_).min, variables(_).min)
  }

  def binary(variables: Traversable[CPIntVar], varHeuris: (CPIntVar => Int), valHeuris: (CPIntVar => Int)): Branching = {
    val vars = variables.toArray
    binaryIdx(vars, (i: Int) => varHeuris(vars(i)), (i: Int) => valHeuris(vars(i)))
  }

  def binaryLastConflict(variables: Array[CPIntVar]): Branching = {
    binaryLastConflict(variables, variables(_).size, variables(_).min)
  }

  def binaryLastConflict(variables: Array[CPIntVar], varHeuristic: (Int => Int)): Branching = {
    binaryLastConflict(variables, varHeuristic, variables(_).min)
  }

  def binaryLastConflict(variables: Array[CPIntVar], varHeuristic: (Int => Int), valHeuristic: (Int => Int)): Branching = {
    new BinaryLastConflict(variables, varHeuristic, valHeuristic)
  }

  def splitLastConflict(variables: Array[CPIntVar]): Branching = {
    splitLastConflict(variables, variables(_).size, variables(_).min)
  }

  def splitLastConflict(variables: Array[CPIntVar], varHeuristic: (Int => Int)): Branching = {
    splitLastConflict(variables, varHeuristic, variables(_).min)
  }

  def splitLastConflict(variables: Array[CPIntVar], varHeuristic: (Int => Int), valHeuristic: (Int => Int)): Branching = {
    new SplitLastConflict(variables, varHeuristic, valHeuristic)
  }

  def conflictOrderingSearch(variables: Array[CPIntVar], varHeuristic: (Int) => Int, valHeuristic: (Int) => Int, doReset: Boolean = false): Branching = {
    new ConflictOrderingSearch(variables, varHeuristic, valHeuristic, doReset)
  }


  /**
   * Binary Search on the decision variables vars with fixed static ordering.
   * The next variable to assign is the first unbound variable in vars.
   * @param vars: the array of variables to assign during the search
   * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
   */
  def binaryStaticIdx(vars: Seq[CPIntVar], valHeuris: Int => Int): Branching = new BinaryStaticOrderBranching(vars.toArray, valHeuris)

  def binaryStatic(vars: Seq[CPIntVar], valHeuris: (CPIntVar => Int)): Branching = new BinaryStaticOrderBranching(vars.toArray, i => valHeuris(vars(i)))

  def binaryStatic(vars: Seq[CPIntVar]): Branching = binaryStatic(vars, (x: CPIntVar) => x.min)

  /**
   * Binary First Fail (min dom size) on the decision variables vars.
   * @param vars: the array of variables to assign during the search
   * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
   */
  def binaryFirstFailIdx(variables: Seq[CPIntVar], valHeuris: (Int => Int)): Branching = {
    val vars = variables.toArray
    binaryIdx(vars, vars(_).size, valHeuris)
  }
  
  def binaryFirstFail(variables: Seq[CPIntVar]): Branching = {
    val vars = variables.toArray    
    binaryFirstFailIdx(vars, vars(_).min)
  }

  def binaryFirstFail(variables: Seq[CPIntVar], valHeuris: (CPIntVar => Int)): Branching = {
    val vars = variables.toArray    
    binaryFirstFailIdx(vars, i => valHeuris(vars(i)))
  }

  /**
   * Binary search on the decision variables vars, selecting first the variables having the max number of propagation methods attached to it.
   */
  def binaryMaxDegree(variables: Seq[CPIntVar]): Branching = {
    val vars = variables.toArray
    binaryIdx(vars, vars(_).constraintDegree, vars(_).min)
  }
  
  /**
   * Binary search on the decision variables vars, splitting the domain of the selected variable on the
   * median of the values (left : <= median, right : > median)
   */
  def binarySplitIdx(x: Seq[CPIntVar], varHeuris: (Int => Int), valHeuris: (Int => Int)): Branching = {
    val xa = x.toArray
    new BinaryDomainSplitBranching(xa, varHeuris, valHeuris)
  }

  def binarySplitIdx(x: Seq[CPIntVar], varHeuris: (Int => Int)): Branching = {
    val xa = x.toArray
    new BinaryDomainSplitBranching(xa, varHeuris)
  }

  def binarySplit(x: Seq[CPIntVar], varHeuris: (CPIntVar => Int), valHeuris: (CPIntVar => Int)): Branching = {
    binarySplitIdx(x, i => varHeuris(x(i)), i => valHeuris(x(i)))
  }

  def binarySplit(x: Seq[CPIntVar], varHeuris: (CPIntVar => Int)): Branching = {
    binarySplitIdx(x, i => varHeuris(x(i)))
  }

  def binarySplit(x: Seq[CPIntVar]): Branching = {
    val xa = x.toArray
    new BinaryDomainSplitBranching(xa, minVar(xa.toArray))
  }

  /**
   * Binary Search on the set variable
   * forcing an arbitrary on the left, and removing it on the right until the variable is bound
   */
  def binary(x: CPSetVar): Branching = {
    new BinarySetBranching(x)
  }

  /**
   * set times heuristic (for discrete resources)
   * see: Time- versus-capacity compromises in project scheduling. (Le Pape et al.). 1994.
   */
  def setTimes(starts: IndexedSeq[CPIntVar], durations: IndexedSeq[CPIntVar], ends: IndexedSeq[CPIntVar], tieBreaker: Int => Int = (i: Int) => i): Branching = new SetTimesBranching(starts, durations, ends, tieBreaker)

  /**
   * rank heuristic (for unary resources)
   */
  def rank[T](starts: IndexedSeq[CPIntVar], durations: IndexedSeq[CPIntVar], ends: IndexedSeq[CPIntVar], by: Int => T)(implicit orderer: T => Ordered[T]): Branching = new RankBranching(starts, durations, ends, by)

  def rank(starts: IndexedSeq[CPIntVar], durations: IndexedSeq[CPIntVar], ends: IndexedSeq[CPIntVar]): Branching = {
    rank(starts, durations, ends, (i: Int) => ends(i).max)
  }

  def discrepancy(branching: Branching, maxDiscrepancy: Int)(implicit context: ReversibleContext): Branching = {
    new DiscrepancyBranching(context, branching, maxDiscrepancy)
  }

  def minDom(x: CPIntVar): Int = x.size

  def minDom(x: Array[CPIntVar]): Int => Int = i => x(i).size

  def minRegret(x: CPIntVar): Int = x.max - x.min

  def minRegret(x: Array[CPIntVar]): Int => Int = i => x(i).max - x(i).min

  def minDomMaxDegree(x: CPIntVar): (Int, Int) = (x.size, -x.constraintDegree)

  def minDomMaxDegree(x: Array[CPIntVar]): Int => (Int, Int) = (i => (x(i).size, -x(i).constraintDegree))

  def minVar(x: CPIntVar): Int = 1

  def minVar(x: Array[CPIntVar]): Int => Int = i => i

  def maxDegree(x: CPIntVar): Int = -x.constraintDegree

  def maxDegree(x: Array[CPIntVar]) = (i: Int) => -x(i).constraintDegree

  def minVal(x: CPIntVar): Int = x.min

  def minVal(x: Array[CPIntVar]) = (i: Int) => x(i).min

  def maxVal(x: CPIntVar): Int = x.max

  def maxVal(x: Array[CPIntVar]) = (i: Int) => x(i).max

  def minValminVal(x: CPIntVar): (Int, Int) = (x.min, x.min)
}
