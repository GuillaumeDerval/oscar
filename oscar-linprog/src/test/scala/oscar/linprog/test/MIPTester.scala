package oscar.linprog.test

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import oscar.algebra._
import oscar.linprog.enums.{Optimal, SolutionFound}
import oscar.linprog.interface.MPSolverLib
import oscar.linprog.modeling._

import scala.util.Success

@RunWith(classOf[JUnitRunner])
class MIPTester extends OscarLinprogTester {

  testForAllSolvers(MPSolverLib.mipSolvers, "Maximize objective under constraints with integer variables") { implicit solver =>
    val x = MPIntVar("x", 0 to 100)
    val y = MPFloatVar("y", 0, 100)

    maximize(100 * x + 1 * y)
    add(3 * x + 1 * y <:= 14.5)

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)
    solver.solutionQuality should equal(Success(Optimal))

    x.value should equalWithTolerance(Some(4.0))
    y.value should equalWithTolerance(Some(14.5 - 3*4))
  }

  // NOTE:
  // this test might fail for LpSolve in case infinity is modified
  testForAllSolvers(MPSolverLib.mipSolvers, "Maximize objective under constraints with integer variables only") { implicit solver =>
    val x = MPIntVar("x", 0, 100)
    val y = MPIntVar("y", 0, 100)

    maximize(100 * x + 1 * y)
    add(3 * x + 1 * y <:= 14.5)

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)
    solver.solutionQuality should equal(Success(Optimal))

    x.value should equalWithTolerance(Some(4.0))
    y.value should equalWithTolerance(Some(2.0))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Change variable type from integer to continuous") { implicit solver =>
    val x = MPIntVar("x", 0 to 100)
    val y = MPFloatVar("y", 0, 100)

    maximize(100 * x + 1 * y)
    add(3 * x + 1 * y <:= 14.5)

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)

    // Set x as a continuous var => performs the linear relaxation of the above problem
    x.setFloat()

    // Both variables should be continuous
    x.float should equal(true)
    y.float should equal(true)

    // Model has changed
    solver.solved should equal(false)
    solver.hasSolution should equal(false)

    // Reoptimize to get new solution
    val endStatus2 = solver.solve

    endStatus2 should equal(SolutionFound)
    solver.solutionQuality should equal(Success(Optimal))

    x.value should equalWithTolerance(Some(14.5/3.0))
    y.value should equalWithTolerance(Some(0.0))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Change variable type from continuous to integer") { implicit solver =>
    val x = MPIntVar("x", 0, 100)
    val y = MPFloatVar("y", 0, 100)
    val z = MPFloatVar("z", 0, 100)

    maximize(1 * x + 2 * y + 3 * z)
    add(x + y <:= 75.5)
    add(x + z <:= 75.5)

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)

    // Set y as an integer var
    y.setInteger()

    // Both variables should be integer
    x.integer should equal(true)
    y.integer should equal(true)
    z.integer should equal(false)

    // Model has changed
    solver.solved should equal(false)
    solver.hasSolution should equal(false)

    // Reoptimize to get new solution
    val endStatus2 = solver.solve

    endStatus2 should equal(SolutionFound)
    solver.solutionQuality should equal(Success(Optimal))

    x.value should equalWithTolerance(Some(0.0))
    y.value should equalWithTolerance(Some(75.0))
    z.value should equalWithTolerance(Some(75.5))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Maximize objective under constraints with binary variables") { implicit solver =>
    val x = MPBinaryVar("x")
    val y = MPFloatVar("y", 0, 100)

    maximize(100 * x + 1 * y)
    add(3 * x + 1 * y <:= 14.5)

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)
    solver.solutionQuality should equal(Success(Optimal))

    x.value should equalWithTolerance(Some(1))
    y.value should equalWithTolerance(Some(14.5 - 3*1))
  }

  for {
    n <- Seq(4, 8, 16)
  } {
    testForAllSolvers(MPSolverLib.mipSolvers, s"Solves the $n Queens problem") { implicit solver =>
      val lines = 0 until n
      val columns = 0 until n

      val x = Array.tabulate(n, n)((l, c) => MPBinaryVar(s"x($l, $c)"))

      maximize(sum(lines, columns) { (l, c) => x(l)(c) })

      /* at most one queen can be placed in each row */
      for (l <- lines)
        add(sum(columns)(c => x(l)(c)) <:= 1)

      /* at most one queen can be placed in each column */
      for (c <- columns)
        add(sum(lines)(l => x(l)(c)) <:= 1)

      /* at most one queen can be placed in each "/"-diagonal  upper half*/
      for (i <- 1 until n)
        add(sum(0 to i)((j) => x(i - j)(j)) <:= 1)

      /* at most one queen can be placed in each "/"-diagonal  lower half*/
      for (i <- 1 until n)
        add(sum(i until n)((j) => x(j)(n - 1 - j + i)) <:= 1)

      /* at most one queen can be placed in each "/"-diagonal  upper half*/
      for (i <- 0 until n)
        add(sum(0 until n - i)((j) => x(j)(j + i)) <:= 1)

      /* at most one queen can be placed in each "/"-diagonal  lower half*/
      for (i <- 1 until n)
        add(sum(0 until n - i)((j) => x(j + i)(j)) <:= 1)

      val endStatus = solver.solve

      endStatus should equal(SolutionFound)
      solver.solutionQuality should equal(Success(Optimal))

      solver.objectiveValue.toOption should equalWithTolerance(Some(n))
    }
  }

}
