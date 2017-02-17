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

import oscar.algo.branchings._
import oscar.algo.search.{Branching, DiscrepancyBranching}
import oscar.algo.vars.IntVarLike
import oscar.cp.searches.WeightedDegreeHelper
import oscar.modeling.models.cp.CPModel
import oscar.modeling.vars.IntVar
import oscar.modeling.vars.cp.CPIntVar

import scala.util.Random

object Branchings {
  //def apply(b: => Seq[Alternative]): BranchingInstantiator = (cp) => new Branching { override def alternatives = b }
  def apply(b: => Seq[Alternative]): BranchingInstantiator = (cp) => new Branching { override def alternatives = b }
  def fromAlternatives(b: () => Seq[Alternative]): BranchingInstantiator = (cp) => new Branching { override def alternatives = b() }
  def fromAlternatives(b: CPModel => Seq[Alternative]): BranchingInstantiator = (cp) => new Branching { override def alternatives = b(cp) }

  type Alternative = Function0[Unit]

  def branch(left: => Unit)(right: => Unit): Seq[Alternative] = Seq(() => left,() => right)

  def branchOne(action: => Unit) = Seq(() => action)

  def branchAll[A](indexes: Seq[A])(f: A => Unit): Seq[Alternative] = {
    indexes.map(i => () => f(i))
  }

  val noAlternative = Seq[Alternative]()

  type BranchingInstantiator = CPModel => Branching

  def binaryIdx[T](variables: Array[IntVar], varHeuristic: Int => T, valHeuristic: Int => Int)(implicit orderer: T => Ordered[T]): BranchingInstantiator = {
    (_) => new BinaryBranching(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic, orderer)
  }

  def binaryIdx[T](variables: Array[IntVar], varHeuristic: (Int => T))(implicit orderer: T => Ordered[T]): BranchingInstantiator = {
    binaryIdx(variables, varHeuristic, variables(_).min)(orderer)
  }

  def binary(variables: Array[IntVar]): BranchingInstantiator = {
    binaryIdx(variables, variables(_).min, variables(_).min)
  }

  def binary(variables: Traversable[IntVar], varHeuris: (IntVar => Int), valHeuris: (IntVar => Int)): BranchingInstantiator = {
    val vars = variables.toArray
    binaryIdx(vars, (i: Int) => varHeuris(vars(i)), (i: Int) => valHeuris(vars(i)))
  }

  def binaryLastConflict(variables: Array[IntVar]): BranchingInstantiator = {
    binaryLastConflict(variables, variables(_).size, variables(_).min)
  }

  def binaryLastConflict[T](variables: Array[IntVar], varHeuristic: (Int => T))(implicit orderer: T => Ordered[T]): BranchingInstantiator = {
    binaryLastConflict(variables, varHeuristic, variables(_).min)(orderer)
  }

  def binaryLastConflict[T](variables: Array[IntVar], varHeuristic: (Int => T), valHeuristic: (Int => Int))(implicit orderer: T => Ordered[T]): BranchingInstantiator = {
    (_) => new BinaryLastConflict(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic, orderer)
  }

  def splitLastConflict(variables: Array[IntVar]): BranchingInstantiator = {
    splitLastConflict(variables, variables(_).size, variables(_).min)
  }

  def splitLastConflict(variables: Array[IntVar], varHeuristic: (Int => Int)): BranchingInstantiator = {
    splitLastConflict(variables, varHeuristic, variables(_).min)
  }

  def splitLastConflict(variables: Array[IntVar], varHeuristic: (Int => Int), valHeuristic: (Int => Int)): BranchingInstantiator = {
    (_) => new SplitLastConflict(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic)
  }

  def conflictOrderingSearch[T](variables: Array[IntVar], varHeuristic: (Int) => T, valHeuristic: (Int) => Int)(implicit orderer: T => Ordered[T]): BranchingInstantiator = {
    (_) => new ConflictOrderingSearch(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic, orderer)
  }


  /**
   * Binary Search on the decision variables vars with fixed static ordering.
   * The next variable to assign is the first unbound variable in vars.
   * @param vars: the array of variables to assign during the search
   * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
   */
  def binaryStaticIdx(vars: Seq[IntVar], valHeuris: Int => Int): BranchingInstantiator = (_) => new BinaryStaticOrderBranching(vars.toArray, valHeuris)

  def binaryStatic(vars: Seq[IntVar]): BranchingInstantiator = binaryStaticIdx(vars, i => vars(i).min)

  /**
    * N-ary Search on the decision variables vars with fixed static ordering.
    * The next variable to assign is the first unbound variable in vars.
    * @param vars: the array of variables to assign during the search
    * @param valHeuris: give the order on which values for a given variable will be tested
    */
  def naryStaticIdx(vars: Seq[IntVar], valHeuris: Int => Seq[Int]): BranchingInstantiator = (_) => new NaryStaticOrderBranching(vars.toArray, valHeuris)

  def naryStatic(vars: Seq[IntVar], valHeuris: (IntVar => Seq[Int])): BranchingInstantiator = (_) => new NaryStaticOrderBranching(vars.toArray, i => valHeuris(vars(i)))

  def naryStatic(vars: Seq[IntVar]): BranchingInstantiator = naryStatic(vars, (x: IntVar) => x.values().toArray.sorted)

  /**
    * Binary First Fail (min dom size) on the decision variables vars.
    * @param variables: the array of variables to assign during the search
    * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
    */
  def binaryFirstFailIdx(variables: Seq[IntVar], valHeuris: (Int => Int)): BranchingInstantiator = {
    val vars = variables.toArray
    binaryIdx(vars, vars(_).size, valHeuris)
  }

  def binaryFirstFail(variables: Seq[IntVar]): BranchingInstantiator = {
    val vars = variables.toArray
    binaryFirstFailIdx(vars, vars(_).min)
  }

  def binaryFirstFail(variables: Seq[IntVar], valHeuris: (IntVar => Int)): BranchingInstantiator = {
    val vars = variables.toArray
    binaryFirstFailIdx(vars, i => valHeuris(vars(i)))
  }

  /**
   * Binary search on the decision variables vars, selecting first the variables having the max number of propagation methods attached to it.
   */
  def binaryMaxDegree(variables: Seq[IntVar]): BranchingInstantiator = {
    val vars = variables.toArray
    (model: CPModel) => {
      val cpvars = vars.map(_.getRepresentative.asInstanceOf[CPIntVar].realCPVar)

      //TODO find a better way to convert the double to an Int
      new BinaryBranching[Int](vars.asInstanceOf[Array[IntVarLike]], -cpvars(_).constraintDegree, vars(_).min, x => x)
    }
  }

  /**
   * Binary Search based on the weighted degree of each variable, the variable with the greater degree being selected first.
   * The weighted degree of a var is the number of times a constraint to which it is linked has been involved in a failure.
   */
  def binaryMaxWeightedDegree(variables: Seq[IntVar], valHeuris: (IntVar => Int), decayRatio: Double): BranchingInstantiator = {
    val vars = variables.toArray
    (model: CPModel) => {
      val cpvars = vars.map(_.getRepresentative.asInstanceOf[CPIntVar].realCPVar)
      val helper = new WeightedDegreeHelper(model.cpSolver, cpvars, decayRatio)

      //TODO find a better way to convert the double to an Int
      new BinaryBranching[Int](vars.asInstanceOf[Array[IntVarLike]], i => -(helper.getWeightedDegree(cpvars(i))*1000).round.toInt, i => valHeuris(vars(i)), x => x)
    }
  }

  def binaryMaxWeightedDegree(variables: Seq[IntVar], decayRatio: Double = 0.99): BranchingInstantiator = {
    binaryMaxWeightedDegree(variables, x => x.min, decayRatio)
  }

  /**
   * Minimize (domain size)/(weighted degree)
   */
  def binaryMinDomOnWeightedDegree(variables: Seq[IntVar], valHeuris: (IntVar => Int), decayRatio: Double): BranchingInstantiator = {
    val vars = variables.toArray
    (model: CPModel) => {
      val cpvars = vars.map(_.getRepresentative.asInstanceOf[CPIntVar].realCPVar)
      val helper = new WeightedDegreeHelper(model.cpSolver, cpvars, decayRatio)

      //TODO find a better way to convert the double to an Int
      new BinaryBranching[Int](vars.asInstanceOf[Array[IntVarLike]], i => (helper.getDomOnWeightedDegree(cpvars(i))*1000).round.toInt, i => valHeuris(vars(i)), x => x)
    }


  }

  def binaryMinDomOnWeightedDegree(variables: Seq[IntVar], decayRatio: Double = 0.99): BranchingInstantiator = {
    binaryMinDomOnWeightedDegree(variables, x => x.min, decayRatio)
  }

  def activityBasedSearch(variables: Seq[IntVar], valHeuristic: Int => Int,rand: Random,nProbes: Int = 1000, decay: Double = 0.999): BranchingInstantiator = {
    (_) => new oscar.algo.branchings.BinaryABS(variables.toArray, valHeuristic, rand, nProbes, decay)
  }

  /**
   * Binary search on the decision variables vars, splitting the domain of the selected variable on the
   * median of the values (left : <= median, right : > median)
   */
  def binarySplitIdx[T](x: Seq[IntVar], varHeuris: (Int => T), valHeuris: (Int => Int))(implicit orderer: T => Ordered[T]): BranchingInstantiator = {
    val xa = x.toArray.asInstanceOf[Array[IntVarLike]]
    (_) => new BinaryDomainSplitBranching(xa, varHeuris, valHeuris, orderer)
  }

  def binarySplitIdx[T](x: Seq[IntVar], varHeuris: (Int => T))(implicit orderer: T => Ordered[T]): BranchingInstantiator = {
    val xa = x.toArray.asInstanceOf[Array[IntVarLike]]
    (_) => new BinaryDomainSplitBranching(xa, varHeuris,orderer)
  }

  def binarySplit(x: Seq[IntVar], varHeuris: (IntVar => Int), valHeuris: (IntVar => Int)): BranchingInstantiator = {
    binarySplitIdx(x, i => varHeuris(x(i)), i => valHeuris(x(i)))
  }

  def binarySplit(x: Seq[IntVar], varHeuris: (IntVar => Int)): BranchingInstantiator = {
    binarySplitIdx(x, i => varHeuris(x(i)))
  }

  def binarySplit(x: Seq[IntVar]): BranchingInstantiator = {
    binarySplitIdx(x,i => i)
  }


  def discrepancy(branching: Branching, maxDiscrepancy: Int): BranchingInstantiator = {
    (_) => new DiscrepancyBranching(branching, maxDiscrepancy)
  }

  def minDom(x: IntVar): Int = x.size

  def minDom(x: Array[IntVar]): Int => Int = i => x(i).size

  def minRegret(x: IntVar): Int = x.max - x.min

  def minRegret(x: Array[IntVar]): Int => Int = i => x(i).max - x(i).min

  def minVal(x: IntVar): Int = x.min

  def minVal(x: Array[IntVar]) = (i: Int) => x(i).min

  def maxVal(x: IntVar): Int = x.max

  def maxVal(x: Array[IntVar]) = (i: Int) => x(i).max

  def minValminVal(x: IntVar): (Int, Int) = (x.min, x.min)
}