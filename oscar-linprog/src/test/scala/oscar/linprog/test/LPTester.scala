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

package oscar.linprog.test

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import oscar.algebra._
import oscar.linprog.enums._
import oscar.linprog.interface.{MPSolverInterface, MPSolverLib}
import oscar.linprog.modeling._
import Migration._
import scala.util.Success



@RunWith(classOf[JUnitRunner])
class LPTester extends OscarLinprogTester {

  testForAllSolvers(MPSolverLib.lpSolvers, "Maximize objective under constraints") { implicit solver =>
    val x = MPFloatVar("x", 100, 150)
    val y = MPFloatVar("y", 80, 170)

    maximize(-2 * x + 5 * y)
    add(x + y <:= 200)

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)

    x.value should equalWithTolerance(Some(100))
    y.value should equalWithTolerance(Some(100))

    solver.objectiveValue should equal(Success(-2 * 100 + 5 * 100))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.lpSolvers, "Minimize objective under constraints") { implicit solver =>
    val x = MPFloatVar("x", 100, 150)
    val y = MPFloatVar("y", 80, 170)

    minimize(-2 * x + 5 * y)
    add(x + y >:= 200)

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)

    x.value should equalWithTolerance(Some(150))
    y.value should equalWithTolerance(Some(80))

    solver.objectiveValue should equal(Success(-2 * 150 + 5 * 80))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.lpSolvers, "Add multiple constraints") { implicit solver =>
    val x = MPFloatVar("x", 0, 100)
    val y = MPFloatVar("y", 0, 100)
    val z = MPFloatVar("z", 0, 100)

    maximize(1 * x + 2 * y + 3 * z)
    add(x + y <:= 75)
    add(x + z <:= 75)

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0))
    y.value should equalWithTolerance(Some(75))
    z.value should equalWithTolerance(Some(75))

    solver.objectiveValue should equal(Success(1 * 0 + 2 * 75 + 3 * 75))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.lpSolvers, "Free variable") { implicit solver =>
    val x = MPFloatVar("x")
    val y = MPFloatVar("y", 0, 100)
    val z = MPFloatVar("z", 0, 100)

    maximize(10 * x + 2 * y + 3 * z)
    add(x + y <:= 75)
    add(x + z <:= 75)
    add(x >:= 0)

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)

    x.value should equalWithTolerance(Some(75))
    y.value should equalWithTolerance(Some(0))
    z.value should equalWithTolerance(Some(0))

    solver.objectiveValue should equal(Success(10 * 75 + 2 * 0 + 3 * 0))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.lpSolvers, "Detect infeasible problem") { implicit solver =>
    val x = MPFloatVar("x", 0, 10)
    val y = MPFloatVar("y", 80, 170)

    minimize(-2 * x + 5 * y)
    add(x + y >:= 200)

    val endStatus = solver.solve

    endStatus should equal(Infeasible)

    intercept[NoSolutionFoundException] {
      solver.solutionQuality
    }
  }

  testForAllSolvers(MPSolverLib.lpSolvers, "Detect unbounded problem") { implicit solver =>
    val x = MPFloatVar("x")
    val y = MPFloatVar("y", 80, 170)

    minimize(-2 * x + 5 * y)
    add(x + y >:= 200)

    val endStatus = solver.solve

    endStatus should (equal(Unbounded) or equal(InfeasibleOrUnbounded))

    intercept[NoSolutionFoundException] {
      solver.solutionQuality
    }
  }

  testForAllSolvers(MPSolverLib.lpSolvers, "Update variable bounds") { implicit solver =>
    val x = MPFloatVar("x", 100, 150)
    val y = MPFloatVar("y", 80, 170)

    maximize(-2 * x + 5 * y)
    add(x + y <:= 200)

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)

    x.value should equalWithTolerance(Some(100))
    y.value should equalWithTolerance(Some(100))

    solver.objectiveValue should equal(Success(-2 * 100 + 5 * 100))
    solver.solutionQuality should equal(Success(Optimal))

    // Update bounds
    x.lowerBound = 0
    y.upperBound = 250

    // Model has changed
    solver.solved should equal(false)
    solver.hasSolution should equal(false)

    // Reoptimize to get new solution
    val endStatus2 = solver.solve

    endStatus2 should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0))
    y.value should equalWithTolerance(Some(200))

    solver.objectiveValue should equal(Success(-2 * 0 + 5 * 200))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.lpSolvers, "Update objective") { implicit solver =>
    val x = MPFloatVar("x", 100, 150)
    val y = MPFloatVar("y", 80, 170)

    maximize(-2 * x + 5 * y)
    add(x + y <:= 200)

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)

    x.value should equalWithTolerance(Some(100))
    y.value should equalWithTolerance(Some(100))

    solver.objectiveValue should equal(Success(-2 * 100 + 5 * 100))
    solver.solutionQuality should equal(Success(Optimal))

    // Update objective
    maximize(2 * x - 5 * y)

    // Model has changed
    solver.solved should equal(false)
    solver.hasSolution should equal(false)

    // Reoptimize to get new solution
    val endStatus2 = solver.solve

    endStatus2 should equal(SolutionFound)

    x.value should equalWithTolerance(Some(120))
    y.value should equalWithTolerance(Some(80))

    solver.objectiveValue should equal(Success(2 * 120 - 5 * 80))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.lpSolvers, "Remove constraint before solve") { implicit solver =>
    val x = MPFloatVar("x", 0, 100)
    val y = MPFloatVar("y", 0, 100)
    val z = MPFloatVar("z", 0, 100)

    maximize(1 * x + 2 * y + 3 * z)
    add(x + y <:= 75, "cstr0")
    add(x + z <:= 75, "cstr1")

    solver.removeLinearConstraint("cstr0")

    add(x + y <:= 60, "cstr0")

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0))
    y.value should equalWithTolerance(Some(60))
    z.value should equalWithTolerance(Some(75))

    solver.objectiveValue.toOption should equalWithTolerance(Success(1 * 0.0 + 2 * 60 + 3 * 75).toOption)
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.lpSolvers, "Remove constraint after solve") { implicit solver =>
    val x = MPFloatVar("x", 0, 100)
    val y = MPFloatVar("y", 0, 100)
    val z = MPFloatVar("z", 0, 100)

    maximize(1 * x + 2 * y + 3 * z)
    add(x + y <:= 75, "cstr0")
    add(x + z <:= 75, "cstr1")

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0))
    y.value should equalWithTolerance(Some(75))
    z.value should equalWithTolerance(Some(75))

    solver.objectiveValue.toOption should equalWithTolerance(Success(1 * 0.0 + 2 * 75 + 3 * 75).toOption)
    solver.solutionQuality should equal(Success(Optimal))

    // update model
    solver.removeLinearConstraint("cstr0")
    add(x + y <:= 60, "cstr0")

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0))
    y.value should equalWithTolerance(Some(60))
    z.value should equalWithTolerance(Some(75))

    solver.objectiveValue.toOption should equalWithTolerance(Success(1 * 0.0 + 2 * 60 + 3 * 75).toOption)
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.lpSolvers, "Remove unused variable before solve") { implicit solver =>
    val x = MPFloatVar("x", 0, 100)
    val y = MPFloatVar("y", 0, 100)
    val w = MPFloatVar("w", 0, 100)
    val z = MPFloatVar("z", 0, 100)

    maximize(1 * x + 2 * y + 3 * z)
    add(x + y <:= 75, "cstr0")
    add(x + z <:= 75, "cstr1")

    solver.removeVariable("w")

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0))
    y.value should equalWithTolerance(Some(75))
    z.value should equalWithTolerance(Some(75))

    solver.objectiveValue.toOption should equalWithTolerance(Success(1 * 0.0 + 2 * 75 + 3 * 75).toOption)
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.lpSolvers, "Remove unused variable after solve") { implicit solver =>
    val x = MPFloatVar("x", 0, 100)
    val y = MPFloatVar("y", 0, 100)
    val w = MPFloatVar("w", 0, 100)
    val z = MPFloatVar("z", 0, 100)

    maximize(1 * x + 2 * y + 3 * z)
    add(x + y <:= 75, "cstr0")
    add(x + z <:= 75, "cstr1")

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0))
    y.value should equalWithTolerance(Some(75))
    z.value should equalWithTolerance(Some(75))

    solver.objectiveValue.toOption should equalWithTolerance(Success(1 * 0.0 + 2 * 75 + 3 * 75).toOption)
    solver.solutionQuality should equal(Success(Optimal))

    solver.removeVariable("w")

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0))
    y.value should equalWithTolerance(Some(75))
    z.value should equalWithTolerance(Some(75))

    solver.objectiveValue.toOption should equalWithTolerance(Success(1 * 0.0 + 2 * 75 + 3 * 75).toOption)
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.lpSolvers, "Cannot remove used variable") { implicit solver =>
    intercept[IllegalArgumentException] {
      val x = MPFloatVar("x", 0, 100)
      val y = MPFloatVar("y", 0, 100)
      val z = MPFloatVar("z", 0, 100)

      maximize(1 * x + 2 * y + 3 * z)
      add(x + y <:= 75)
      add(x + z <:= 75)

      solver.solve

      solver.removeVariable("y")
    }
  }
}
