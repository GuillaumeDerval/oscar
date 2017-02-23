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

import oscar.algo.branchings._
import oscar.algo.search.{Branching, BranchingUtils, DiscrepancyBranching}
import oscar.cp.scheduling.search.SetTimesBranching
import oscar.cp.scheduling.search.RankBranching
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPSetVar
import oscar.algo.vars.{IntVarLike, SetVarLike}
import oscar.cp._
import oscar.cp.searches.WeightedDegreeHelper

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
trait Branchings extends BranchingUtils {
  @inline
  implicit def arrayCPIntToIntLike(orig: Array[CPIntVar]): Array[IntVarLike] = orig.asInstanceOf[Array[IntVarLike]]
  @inline
  implicit def arrayCPSetToSetLike(orig: Array[CPSetVar]): Array[SetVarLike] = orig.asInstanceOf[Array[SetVarLike]]

  def binaryIdx[T](variables: Array[CPIntVar], varHeuristic: Int => T, valHeuristic: Int => Int)(implicit orderer: T => Ordered[T]): Branching = {
    new BinaryBranching(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic, orderer)
  }

  def binaryIdx[T](variables: Array[CPIntVar], varHeuristic: (Int => T))(implicit orderer: T => Ordered[T]): Branching = {
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

  def binaryLastConflict[T](variables: Array[CPIntVar], varHeuristic: (Int => T))(implicit orderer: T => Ordered[T]): Branching = {
    binaryLastConflict(variables, varHeuristic, variables(_).min)(orderer)
  }

  def binaryLastConflict[T](variables: Array[CPIntVar], varHeuristic: (Int => T), valHeuristic: (Int => Int))(implicit orderer: T => Ordered[T]): Branching = {
    new BinaryLastConflict[T](variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic,orderer)
  }

  def splitLastConflict(variables: Array[CPIntVar]): Branching = {
    splitLastConflict(variables, variables(_).size, variables(_).min)
  }

  def splitLastConflict(variables: Array[CPIntVar], varHeuristic: (Int => Int)): Branching = {
    splitLastConflict(variables, varHeuristic, variables(_).min)
  }

  def splitLastConflict(variables: Array[CPIntVar], varHeuristic: (Int => Int), valHeuristic: (Int => Int)): Branching = {
    new SplitLastConflict(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic)
  }

  def conflictOrderingSearch[T](variables: Array[CPIntVar], varHeuristic: (Int) => T, valHeuristic: (Int) => Int)(implicit orderer: T => Ordered[T]): Branching = {
    new ConflictOrderingSearch(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic, orderer)
  }


  /**
   * Binary Search on the decision variables vars with fixed static ordering.
   * The next variable to assign is the first unbound variable in vars.
   * @param vars: the array of variables to assign during the search
   * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
   */
  def binaryStaticIdx(vars: Seq[CPIntVar], valHeuris: Int => Int): Branching = new BinaryStaticOrderBranching(vars.toArray, valHeuris)

  def binaryStatic(vars: Seq[CPIntVar]): Branching = binaryStaticIdx(vars, i => vars(i).min)

  /**
   * Binary First Fail (min dom size) on the decision variables vars.
   * @param variables: the array of variables to assign during the search
   * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
   */
  def binaryFirstFailIdx(variables: Seq[CPIntVar], valHeuris: (Int => Int)): Branching = {
    val vars = variables.toArray
    binaryIdx(vars, i => (vars(i).size,i), valHeuris)
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
    binaryIdx(vars, -vars(_).constraintDegree, vars(_).min)
  }

  /**
   * Binary Search based on the weighted degree of each variable, the variable with the greater degree being selected first.
   * The weighted degree of a var is the number of times a constraint to which it is linked has been involved in a failure.
   */
  def binaryMaxWeightedDegree(variables: Seq[CPIntVar], valHeuris: (CPIntVar => Int), decayRatio: Double): Branching = {
    val vars = variables.toArray
    val helper = new WeightedDegreeHelper(vars.head.store, vars, decayRatio)

    //TODO find a better way to convert the double to an Int
    binaryIdx(vars, i => -(helper.getWeightedDegree(vars(i))*1000).round.toInt, i => valHeuris(vars(i)))
  }

  def binaryMaxWeightedDegree(variables: Seq[CPIntVar], decayRatio: Double = 0.99): Branching = {
    binaryMaxWeightedDegree(variables, x => x.min, decayRatio)
  }

  /**
   * Minimize (domain size)/(weighted degree)
   */
  def binaryMinDomOnWeightedDegree(variables: Seq[CPIntVar], valHeuris: (CPIntVar => Int), decayRatio: Double): Branching = {
    val vars = variables.toArray
    val helper = new WeightedDegreeHelper(vars.head.store, vars, decayRatio)

    //TODO find a better way to convert the double to an Int
    binaryIdx(vars, i => (helper.getDomOnWeightedDegree(vars(i))*1000).round.toInt, i => valHeuris(vars(i)))
  }

  def binaryMinDomOnWeightedDegree(variables: Seq[CPIntVar], decayRatio: Double = 0.99): Branching = {
    binaryMinDomOnWeightedDegree(variables, x => x.min, decayRatio)
  }

  /**
   * Binary search on the decision variables vars, splitting the domain of the selected variable on the
   * median of the values (left : <= median, right : > median)
   */
  def binarySplitIdx[T](x: Seq[CPIntVar], varHeuris: (Int => T), valHeuris: (Int => Int))(implicit orderer: T => Ordered[T]): Branching = {
    val xa = x.toArray.asInstanceOf[Array[IntVarLike]]
    new BinaryDomainSplitBranching(xa, varHeuris, valHeuris, orderer)
  }

  def binarySplitIdx[T](x: Seq[CPIntVar], varHeuris: (Int => T))(implicit orderer: T => Ordered[T]): Branching = {
    val xa = x.toArray.asInstanceOf[Array[IntVarLike]]
    new BinaryDomainSplitBranching(xa, varHeuris,orderer)
  }

  def binarySplit(x: Seq[CPIntVar], varHeuris: (CPIntVar => Int), valHeuris: (CPIntVar => Int)): Branching = {
    binarySplitIdx(x, i => varHeuris(x(i)), i => valHeuris(x(i)))
  }

  def binarySplit(x: Seq[CPIntVar], varHeuris: (CPIntVar => Int)): Branching = {
    binarySplitIdx(x, i => varHeuris(x(i)))
  }

  def binarySplit(x: Seq[CPIntVar]): Branching = {
    binarySplitIdx(x,i => i)
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

  def discrepancy(branching: Branching, maxDiscrepancy: Int): Branching = {
    new DiscrepancyBranching(branching, maxDiscrepancy)
  }

  def minDom(x: CPIntVar): Int = x.size

  def minDom(x: Array[CPIntVar]): Int => Int = i => x(i).size

  def minRegret(x: CPIntVar): Int = x.max - x.min

  def minRegret(x: Array[CPIntVar]): Int => Int = i => x(i).max - x(i).min

  def minDomMaxDegree(x: CPIntVar): (Int, Int) = (x.size, -x.constraintDegree)

  def minDomMaxDegree(x: Array[CPIntVar]): Int => (Int, Int) = (i => (x(i).size, -x(i).constraintDegree))

  def maxDegree(x: CPIntVar): Int = -x.constraintDegree

  def maxDegree(x: Array[CPIntVar]) = (i: Int) => -x(i).constraintDegree

  def minVal(x: CPIntVar): Int = x.min

  def minVal(x: Array[CPIntVar]) = (i: Int) => x(i).min

  def maxVal(x: CPIntVar): Int = x.max

  def maxVal(x: Array[CPIntVar]) = (i: Int) => x(i).max

  def minValminVal(x: CPIntVar): (Int, Int) = (x.min, x.min)

  /**
    * @author Pierre Schaus pschaus@gmail.com
    * @param x
    * @param fallBackValHeuristic
    */
  class ValueHeuristicLearner(x: Array[CPIntVar], fallBackValHeuristic: (Int => Int)){
    private[this] val lastValues = Array.fill(x.length)(Int.MinValue)

    x(0).store.onPush {
      for (i <- 0 until x.length) {
        if (x(i).isBound) {
          lastValues(i) = x(i).min
        }
      }
    }
    def valueHeuristic(i: Int): Int = {
      if (x(i).hasValue(lastValues(i))) {
        lastValues(i)
      } else {
        fallBackValHeuristic(i)
      }
    }
  }

  /**
    * Value Heuristic wrapper that will try to learn a successfull heuristic
    * Basically when a value succeeds, it is recorded and first attempted
    * whenever it is possible
    * @param x the variables on which the defaultValueHeuristic is
    * @param fallBackValHeuristic i => v where i is the variable index, v the value in the domain of x(i)
    * @return a value heuristic i => v where i is the variable index, v is the value in the domain of x(i)
    */
  def learnValueHeuristic(x: Array[CPIntVar], fallBackValHeuristic: (Int => Int)): (Int => Int) = {
    new ValueHeuristicLearner(x,fallBackValHeuristic).valueHeuristic
  }
}
