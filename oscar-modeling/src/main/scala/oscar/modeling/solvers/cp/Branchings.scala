package oscar.modeling.solvers.cp

import oscar.algo.branchings._
import oscar.algo.search.{Branching, DiscrepancyBranching}
import oscar.algo.vars.IntVarLike
import oscar.modeling.models.CPModel
import oscar.modeling.vars.IntVar

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

  def binaryIdx(variables: Array[IntVar], varHeuristic: (Int => Int), valHeuristic: (Int => Int)): BranchingInstantiator = {
    (_) => new BinaryBranching(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic)
  }

  def binaryIdx(variables: Array[IntVar], varHeuristic: (Int => Int)): BranchingInstantiator = {
    binaryIdx(variables, varHeuristic, variables(_).min)
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

  def binaryLastConflict(variables: Array[IntVar], varHeuristic: (Int => Int)): BranchingInstantiator = {
    binaryLastConflict(variables, varHeuristic, variables(_).min)
  }

  def binaryLastConflict(variables: Array[IntVar], varHeuristic: (Int => Int), valHeuristic: (Int => Int)): BranchingInstantiator = {
    (_) => new BinaryLastConflict(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic)
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

  def conflictOrderingSearch(variables: Array[IntVar], varHeuristic: (Int) => Int, valHeuristic: (Int) => Int, doReset: Boolean = false): BranchingInstantiator = {
    (_) => new ConflictOrderingSearch(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic, doReset)
  }


  /**
    * Binary Search on the decision variables vars with fixed static ordering.
    * The next variable to assign is the first unbound variable in vars.
    * @param vars: the array of variables to assign during the search
    * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
    */
  def binaryStaticIdx(vars: Seq[IntVar], valHeuris: Int => Int): BranchingInstantiator = (_) => new BinaryStaticOrderBranching(vars.toArray, valHeuris)

  def binaryStatic(vars: Seq[IntVar], valHeuris: (IntVar => Int)): BranchingInstantiator = (_) => new BinaryStaticOrderBranching(vars.toArray, i => valHeuris(vars(i)))

  def binaryStatic(vars: Seq[IntVar]): BranchingInstantiator = binaryStatic(vars, (x: IntVar) => x.min)

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
    * Binary search on the decision variables vars, splitting the domain of the selected variable on the
    * median of the values (left : <= median, right : > median)
    */
  def binarySplitIdx(x: Seq[IntVar], varHeuris: (Int => Int), valHeuris: (Int => Int)): BranchingInstantiator = {
    val xa = x.toArray.asInstanceOf[Array[IntVarLike]]
    (_) => new BinaryDomainSplitBranching(xa, varHeuris, valHeuris)
  }

  def binarySplitIdx(x: Seq[IntVar], varHeuris: (Int => Int)): BranchingInstantiator = {
    val xa = x.toArray.asInstanceOf[Array[IntVarLike]]
    (_) => new BinaryDomainSplitBranching(xa, varHeuris)
  }

  def binarySplit(x: Seq[IntVar], varHeuris: (IntVar => Int), valHeuris: (IntVar => Int)): BranchingInstantiator = {
    binarySplitIdx(x, i => varHeuris(x(i)), i => valHeuris(x(i)))
  }

  def binarySplit(x: Seq[IntVar], varHeuris: (IntVar => Int)): BranchingInstantiator = {
    binarySplitIdx(x, i => varHeuris(x(i)))
  }

  def binarySplit(x: Seq[IntVar]): BranchingInstantiator = {
    val xa = x.toArray
    (_) => new BinaryDomainSplitBranching(xa.asInstanceOf[Array[IntVarLike]], minVar(xa))
  }

  def discrepancy(branching: Branching, maxDiscrepancy: Int): BranchingInstantiator = {
    (_) => new DiscrepancyBranching(branching, maxDiscrepancy)
  }

  def minDom(x: IntVar): Int = x.size

  def minDom(x: Array[IntVar]): Int => Int = i => x(i).size

  def minRegret(x: IntVar): Int = x.max - x.min

  def minRegret(x: Array[IntVar]): Int => Int = i => x(i).max - x(i).min

  def minVar(x: IntVar): Int = 1

  def minVar(x: Array[IntVar]): Int => Int = i => i

  def minVal(x: IntVar): Int = x.min

  def minVal(x: Array[IntVar]) = (i: Int) => x(i).min

  def maxVal(x: IntVar): Int = x.max

  def maxVal(x: Array[IntVar]) = (i: Int) => x(i).max

  def minValminVal(x: IntVar): (Int, Int) = (x.min, x.min)
}