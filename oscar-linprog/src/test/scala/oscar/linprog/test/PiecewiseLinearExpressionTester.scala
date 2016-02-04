package oscar.linprog.test

import java.nio.file.Paths

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import oscar.linprog.enums.{LP, Optimal, SolutionFound}
import oscar.linprog.interface.MPSolverLib
import oscar.linprog.modeling._

import scala.util.Success

@RunWith(classOf[JUnitRunner])
class PiecewiseLinearExpressionTester extends OscarLinprogTester {
/*
  testForAllSolvers(MPSolverLib.mipSolvers, "Minimize |x|") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)

    minimize(abs(x, -100, 100))

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0.0))

    solver.objectiveValue.toOption should equalWithTolerance(Some(0.0))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Minimize |-x|") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)

    minimize(abs(-x, -100, 100))

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0.0))

    solver.objectiveValue.toOption should equalWithTolerance(Some(0.0))
    solver.solutionQuality should equal(Success(Optimal))
  }
*/
  testForAllSolvers(MPSolverLib.mipSolvers, "Maximize |x| in [-100; 50]") { implicit solver =>
    val x = MPFloatVar("x", -100, 50)

    maximize(abs(x, -100, 100))

    val path = Paths.get("/Users/acr/Workspaces/oscar/test.lp")
    solver.exportModel(path, LP)

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(-100.0))

    solver.objectiveValue.toOption should equalWithTolerance(Some(100.0))
    solver.solutionQuality should equal(Success(Optimal))
  }
/*
  testForAllSolvers(MPSolverLib.mipSolvers, "Maximize |x| in [-50; 100]") { implicit solver =>
    val x = MPFloatVar("x", -50, 100)

    maximize(abs(x, -100, 100))

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(100.0))

    solver.objectiveValue.toOption should equalWithTolerance(Some(100.0))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Minimize y s.t. y >= |x| with y, x in [-100; 100]") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)
    val y = MPFloatVar("y", -100, 100)

    minimize(y)
    subjectTo(
      "aboveAbsX" -> (y >:= abs(x, -100, 100))
    )

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0.0))
    y.value should equalWithTolerance(Some(0.0))

    solver.objectiveValue.toOption should equalWithTolerance(Some(0.0))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Remove abs expression") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)
    val y = MPFloatVar("y", -100, 100)

    val absX = solver.addAbsExpression(x, -100, 100)

    minimize(y)
    subjectTo(
      "aboveAbsX" -> (y >:= solver.absLinearExpression(absX))
    )

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0.0))
    y.value should equalWithTolerance(Some(0.0))

    solver.objectiveValue.toOption should equalWithTolerance(Some(0.0))
    solver.solutionQuality should equal(Success(Optimal))

    // Remove expression and solve again
    solver.removeLinearConstraint("aboveAbsX")
    solver.removeAbsExpression(absX)

    solver.solve should equal(SolutionFound)

    y.value should equalWithTolerance(Some(-100.0))

    solver.objectiveValue.toOption should equalWithTolerance(Some(-100.0))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Minimize sign(x)") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)

    minimize(sign(x, -100, 100))

    solver.solve should equal(SolutionFound)

    x.value.get should be < 0.0

    solver.objectiveValue.toOption should equalWithTolerance(Some(-1))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Maximize sign(x)") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)

    maximize(sign(x, -100, 100))

    solver.solve should equal(SolutionFound)

    x.value.get should be > 0.0

    solver.objectiveValue.toOption should equalWithTolerance(Some(1))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Minimize |sign(x) - sign(y)| - x") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)
    val y = MPFloatVar("y", -100, 100)

    minimize(abs(sign(x, -100, 100) - sign(y, -100, 100), -100, 100) - x)

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(100))
    y.value.get should be > 0.0

    solver.objectiveValue.toOption should equalWithTolerance(Some(-100.0))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Maximize |sign(x) - sign(y)| + x") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)
    val y = MPFloatVar("y", -100, 100)

    maximize(abs(sign(x, -100, 100) - sign(y, -100, 100), -100, 100) + x)

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(100))
    y.value.get should be < 0.0

    solver.objectiveValue.toOption should equalWithTolerance(Some(102.0))
    solver.solutionQuality should equal(Success(Optimal))
  }
  */
}
