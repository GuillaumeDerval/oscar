package oscar.modeling.solvers.cp.branchings

import oscar.modeling.models.CPModel
import oscar.modeling.vars.IntVar

abstract class Branching extends oscar.algo.search.Branching

object Branching {
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

  /*
   Binary Branchings
   */

  def binaryIdx(variables: Array[IntVar], varHeuristic: (Int => Int), valHeuristic: (Int => Int)): BranchingInstantiator = {
    (cp) => new BinaryBranching(cp, variables, varHeuristic, valHeuristic)
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

  /*
   Binary Last Conflict branchings
   */

  def binaryLastConflict(variables: Array[IntVar]): BranchingInstantiator = {
    binaryLastConflict(variables, variables(_).size, variables(_).min)
  }

  def binaryLastConflict(variables: Array[IntVar], varHeuristic: (Int => Int)): BranchingInstantiator = {
    binaryLastConflict(variables, varHeuristic, variables(_).min)
  }

  def binaryLastConflict(variables: Array[IntVar], varHeuristic: (Int => Int), valHeuristic: (Int => Int)): BranchingInstantiator = {
    (cp) => new BinaryLastConflict(cp, variables, varHeuristic, valHeuristic)
  }

  /*
   Binary Static
   */
  def binaryStaticIdx(vars: Seq[IntVar], valHeuris: Int => Int): BranchingInstantiator = (cp) => new BinaryStaticOrderBranching(cp, vars.toArray, valHeuris)

  def binaryStatic(vars: Seq[IntVar], valHeuris: (IntVar => Int)): BranchingInstantiator = (cp) => new BinaryStaticOrderBranching(cp, vars.toArray, i => valHeuris(vars(i)))

  def binaryStatic(vars: Seq[IntVar]): BranchingInstantiator = binaryStatic(vars, (x: IntVar) => x.min)

  /*
   N-ary branchings
   */

  def naryStatic(variables: Array[IntVar]): BranchingInstantiator = {
    naryStatic(variables, (i) => variables(i).toSeq)
  }

  def naryStatic(variables: Array[IntVar], valOrder: Int => Seq[Int]): BranchingInstantiator = {
    (cp) => new NaryStaticOrderBranching(cp, variables, valOrder)
  }
}