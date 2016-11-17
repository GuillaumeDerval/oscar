package oscar.linprog

import org.scalactic.TripleEqualsSupport.Spread
import org.scalatest._
import oscar.algebra.{Linear, Model, Optimal, Solution, SolveResult, SolveResultWithSolution, SolverInterface}
import oscar.linprog.lp_solve.LPSolve

import scala.collection.immutable.IndexedSeq


abstract class LinearMathSolverTests extends FunSuite {

  private def canInstantiate(f: => Option[SolverInterface[Linear,Linear,Double]]): Option[SolverInterface[Linear,Linear,Double]] =
    try {
      f
    } catch {
      case _: UnsatisfiedLinkError => None
      case _: NoClassDefFoundError => None
      case t: Throwable => fail(t)
    }

  val lpsolve = canInstantiate {
    val run = LPSolve.run(new Model[Linear,Linear,Double]())
    run.release()
    Some(LPSolve)
  }

  override def nestedSuites: IndexedSeq[Suite] =
    IndexedSeq(
      testSuite(lpsolve, "LPSolve")
    )

  def testSuite(interface: Option[SolverInterface[Linear,Linear,Double]], solverName: String): FunSuite
}

abstract class LinearMathSolverTester(interfaceOpt: Option[SolverInterface[Linear, Linear, Double]], val solverName: String) extends FunSuite with Matchers {

  implicit def solverInterface: SolverInterface[Linear, Linear, Double] = interfaceOpt.getOrElse( cancel() )

  def moreOrLess(d: Double): Spread[Double] = d +- 1e-6

  override def test(testName: String, testTags: Tag*)(testFun: => Unit): Unit = interfaceOpt match {
    case Some(i) => super.test(testName)(testFun)
    case None    => ignore(testName)(testFun)
  }

  implicit class ExtendedSolveResult[O <: oscar.algebra.ExpressionDegree, C <: oscar.algebra.ExpressionDegree, V](val solveResult: SolveResult[O,C,V]) {
    def checkSolution(testFun: Solution[V] => Unit): Unit = solveResult match {
      case withSol: SolveResultWithSolution[O,C,V] => testFun(withSol.solution)
      case _ => fail("The SolveResult should have a solution but was " + solveResult)
    }

    def checkOptimalSolution(testFun: Solution[V] => Unit): Unit = solveResult match {
      case optimal: Optimal[O,C,V] => testFun(optimal.solution)
      case _ => fail("The SolveResult should be Optimal but was " + solveResult)
    }
  }
}