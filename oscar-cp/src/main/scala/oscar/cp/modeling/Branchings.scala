/** *****************************************************************************
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
  * *****************************************************************************/

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


  /**
    * Binary search with custom variable/value heuristic
    *
    * @param variables
    * @param varHeuristic given an index in variables, returns an ordered value such that
    *                     the variable with the smallest one is selected first
    * @param valHeuristic given an index i in variables,
    *                     returns the value in domain of variables(i) to be tried on the left branch,
    *                     this value is removed on the right branch
    * @param orderer
    * @tparam T
    * @return a branching implementing the variable-value heuristic as specified in parameter
    */
  def binaryIdx[T](variables: Array[CPIntVar], varHeuristic: Int => T, valHeuristic: Int => Int)(implicit orderer: T => Ordered[T]): Branching = {
    new BinaryBranching(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic, orderer)
  }

  /**
    * @param variables
    * @param varHeuristic given an index in variables, returns an ordered value such
    *                     that the unbound variable with the smallest one is selected first,
    *                     its min value is then tried on the left branch and removed on the right branch
    * @param orderer
    * @tparam T
    * @return a branching implementing the variable-value heuristic as specified in parameter
    */
  def binaryIdx[T](variables: Array[CPIntVar], varHeuristic: (Int => T))(implicit orderer: T => Ordered[T]): Branching = {
    binaryIdx(variables, varHeuristic, variables(_).min)
  }


  /**
    * Static ordered search, take the first unbound var in variables,
    * try its minimum value on the left branch and remove this value on the right branch
    *
    * @param variables
    * @return a branching implementing the variable-value heuristic as specified in parameter
    */
  def binary(variables: Array[CPIntVar]): Branching = {
    binaryIdx(variables, variables(_).min, variables(_).min)
  }

  /**
    *
    * @param variables
    * @param varHeuris given a variable in variables, returns a value (such as the domain size) such that
    *                  the unbound variable with the smallest one is selected first
    * @param valHeuris given the selected variable,
    *                  returns the value in domain of variables(i) to be tried on the left branch,
    *                  this value is removed on the right branch
    * @return a branching implementing the variable-value heuristic as specified in parameter
    */
  def binary(variables: Traversable[CPIntVar], varHeuris: (CPIntVar => Int), valHeuris: (CPIntVar => Int)): Branching = {
    val vars = variables.toArray
    binaryIdx(vars, (i: Int) => varHeuris(vars(i)), (i: Int) => valHeuris(vars(i)))
  }

  /**
    * Variable Heuristic described in
    * Reasoning from last conflict (s) in constraint programming,
    * C Lecoutre, L Saïs, S Tabary, V Vidal, 2009
    * In case a failure occurs after having selected a variable,
    * this variable is tried first until it eventually succeeds
    * and in this case a first-fail (min dom) fall back heuristic is used.
    *
    * @param variables
    * @return A branching implementing last conflict heuristic for the variable,
    *         with first fail fallback var heuristic and min-value for the value heuristic.
    */
  def binaryLastConflict(variables: Array[CPIntVar]): Branching = {
    binaryLastConflict(variables, variables(_).size, variables(_).min)
  }

  /**
    * Variable Heuristic described in
    * Reasoning from last conflict (s) in constraint programming,
    * C Lecoutre, L Saïs, S Tabary, V Vidal, 2009
    * In case a failure occurs after having selected a variable,
    * this variable is tried first until it eventually succeeds
    * and in this case a first-fail (min dom) fall back heuristic is used.
    *
    * @param variables
    * @param varHeuristic fallback heuristic: given an index in variables, returns an ordered value such that
    *                     the variable with the smallest one is selected first
    * @param orderer
    * @tparam T
    * @return
    */
  def binaryLastConflict[T](variables: Array[CPIntVar], varHeuristic: (Int => T))(implicit orderer: T => Ordered[T]): Branching = {
    binaryLastConflict(variables, varHeuristic, variables(_).min)(orderer)
  }

  /**
    * Variable Heuristic described in
    * Reasoning from last conflict (s) in constraint programming,
    * C Lecoutre, L Saïs, S Tabary, V Vidal, 2009
    * In case a failure occurs after having selected a variable,
    * this variable is tried first until it eventually succeeds
    * and in this case a first-fail (min dom) fall back heuristic is used.
    *
    * @param variables
    * @param varHeuristic fallback heuristic: given an index in variables, returns an ordered value such that
    *                     the variable with the smallest one is selected first
    * @param valHeuristic given an index i in variables,
    *                     returns the value in domain of variables(i) to be tried on the left branch,
    *                     this value is removed on the right branch
    * @param orderer
    * @tparam T
    * @return A branching implementing last conflict heuristic for the variable,
    *         with first fail fallback var heuristic and custom value heuristic.
    */
  def binaryLastConflict[T](variables: Array[CPIntVar], varHeuristic: (Int => T), valHeuristic: (Int => Int))(implicit orderer: T => Ordered[T]): Branching = {
    new BinaryLastConflict[T](variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic, orderer)
  }


  def splitLastConflict(variables: Array[CPIntVar]): Branching = {
    splitLastConflict(variables, variables(_).size, variables(_).min)
  }

  def splitLastConflict(variables: Array[CPIntVar], varHeuristic: (Int => Int)): Branching = {
    splitLastConflict(variables, varHeuristic, variables(_).min)
  }

  /**
    * Variable Heuristic described in
    * Reasoning from last conflict (s) in constraint programming,
    * C Lecoutre, L Saïs, S Tabary, V Vidal, 2009
    * In case a failure occurs after having selected a variable,
    * this variable is tried first until it eventually succeeds
    * and in this case a first-fail (min dom) fall back heuristic is used.
    *
    * @param variables
    * @param varHeuristic fallback heuristic: given an index in variables, returns a value (e.g. the domain size) such that
    *                     the variable with the smallest one is selected first
    * @param valHeuristic given an index i in variables,
    *                     returns a value v in domain of variables(i).
    *                     The constraint <= v is tried on the left branch,
    *                     and the constraint > v on the right branch
    * @return A branching implementing last conflict heuristic for the variable,
    *         with first fail fallback var heuristic and custom value split heuristic.
    */
  def splitLastConflict(variables: Array[CPIntVar], varHeuristic: (Int => Int), valHeuristic: (Int => Int)): Branching = {
    new SplitLastConflict(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic)
  }

  /**
    * Variable Heuristic called COS described in
    * Conflict ordering search for scheduling problems
    * S Gay, R Hartert, C Lecoutre, P Schaus, 2015
    * Each variable has a time-stamp according to its last failure.
    * Variables are first tried according to the latest time-stamp as variable heuristic.
    * In case none of the unbound variable has yet caused a failure a fallback heuristic is used.
    *
    * @param variables
    * @param varHeuristic fallback heuristic: given an index in variables, returns an ordered value such that
    *                     the variable with the smallest one is selected first
    * @param valHeuristic given an index i in variables,
    *                     returns the value in domain of variables(i) to be tried on the left branch,
    *                     this value is removed on the right branch
    * @param orderer
    * @tparam T
    * @return A branching implementing conflict Ordering Search (COS) for the variable,
    *         with first fail fallback var heuristic and custom value value heuristic.
    */
  def conflictOrderingSearch[T](variables: Array[CPIntVar], varHeuristic: (Int) => T, valHeuristic: (Int) => Int)(implicit orderer: T => Ordered[T]): Branching = {
    new ConflictOrderingSearch(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic, orderer)
  }


  /**
    * Binary Search on the decision variables vars with fixed static ordering.
    * The next variable to assign is the first unbound variable in vars.
    *
    * @param vars         : the array of variables to assign during the search
    * @param valHeuristic given an index i in variables,
    *                     returns the value in domain of variables(i) to be tried on the left branch,
    *                     this value is removed on the right branch
    */
  def binaryStaticIdx(vars: Seq[CPIntVar], valHeuristic: Int => Int): Branching = new BinaryStaticOrderBranching(vars.toArray, valHeuristic)

  def binaryStatic(vars: Seq[CPIntVar]): Branching = binaryStaticIdx(vars, i => vars(i).min)

  /**
    * Binary First Fail (min dom size) on the decision variables vars.
    *
    * @param variables : the array of variables to assign during the search
    * @param valHeuris : Given an index i in the gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
    */
  def binaryFirstFailIdx(variables: Seq[CPIntVar], valHeuris: (Int => Int)): Branching = {
    val vars = variables.toArray
    binaryIdx(vars, i => (vars(i).size, i), valHeuris)
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
    * Heuristic Decribed in:
    * Boosting systematic search by weighting constraints
    * Frédéric Boussemart, Fred Hemery, Christophe Lecoutre, Lakhdar Sais, 2004
    *
    * Binary Search based on the weighted degree of each variable, the variable with the greater degree being selected first.
    * The weighted degree of a var is the number of times a constraint to which it is linked has been involved in a failure.
    */
  def binaryMaxWeightedDegree(variables: Seq[CPIntVar], valHeuris: (CPIntVar => Int), decayRatio: Double): Branching = {
    val vars = variables.toArray
    val helper = new WeightedDegreeHelper(vars.head.store, vars, decayRatio)

    //TODO find a better way to convert the double to an Int
    binaryIdx(vars, i => -(helper.getWeightedDegree(vars(i)) * 1000).round.toInt, i => valHeuris(vars(i)))
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
    binaryIdx(vars, i => (helper.getDomOnWeightedDegree(vars(i)) * 1000).round.toInt, i => valHeuris(vars(i)))
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
    new BinaryDomainSplitBranching(xa, varHeuris, orderer)
  }

  def binarySplit(x: Seq[CPIntVar], varHeuris: (CPIntVar => Int), valHeuris: (CPIntVar => Int)): Branching = {
    binarySplitIdx(x, i => varHeuris(x(i)), i => valHeuris(x(i)))
  }

  def binarySplit(x: Seq[CPIntVar], varHeuris: (CPIntVar => Int)): Branching = {
    binarySplitIdx(x, i => varHeuris(x(i)))
  }

  def binarySplit(x: Seq[CPIntVar]): Branching = {
    binarySplitIdx(x, i => i)
  }

  /**
    * Binary Search on the set variable
    * forcing an arbitrary on the left, and removing it on the right until the variable is bound
    */
  def binary(x: CPSetVar): Branching = {
    new BinarySetBranching(x)
  }

  /**
    * Set times heuristic (for discrete resources)
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
  class ValueHeuristicLearner(x: Array[CPIntVar], fallBackValHeuristic: (Int => Int)) {
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
    *
    * @param x                    the variables on which the defaultValueHeuristic is
    * @param fallBackValHeuristic i => v where i is the variable index, v the value in the domain of x(i)
    * @return a value heuristic i => v where i is the variable index, v is the value in the domain of x(i)
    */
  def learnValueHeuristic(x: Array[CPIntVar], fallBackValHeuristic: (Int => Int)): (Int => Int) = {
    new ValueHeuristicLearner(x, fallBackValHeuristic).valueHeuristic
  }
}
