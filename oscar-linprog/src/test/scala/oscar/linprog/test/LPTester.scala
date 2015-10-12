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

package oscar.linprog.test

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import oscar.linprog.enums._
import oscar.linprog.modeling._
import oscar.algebra._
import org.scalatest.Matchers

import scala.util.Success

@RunWith(classOf[JUnitRunner])
class LPTester extends FunSuite with Matchers with OscarLinprogMatchers {

  test("Maximize objective under constraints") {
    for (_solver <- MPSolver.lpSolvers) {
      implicit val solver: MPSolver[_] = _solver

      val x = FloatVar("x", 100, 150)
      val y = FloatVar("y", 80, 170)

      maximize(-2 * x + 5 * y)
      add(x + y <= 200)

      val endStatus = solver.solve

      endStatus should equal(SolutionFound)

      x.value should equalWithTolerance(Some(100))
      y.value should equalWithTolerance(Some(100))

      solver.objectiveValue should equal(Success(-2*100 + 5*100))
      solver.solutionQuality should equal(Success(Optimal))

      solver.release()
    }
  }

  test("Minimize objective under constraints") {
    for (_solver <- MPSolver.lpSolvers) {
      implicit val solver: MPSolver[_] = _solver

      val x = FloatVar("x", 100, 150)
      val y = FloatVar("y", 80, 170)

      minimize(-2 * x + 5 * y)
      add(x + y >= 200)

      val endStatus = solver.solve

      endStatus should equal(SolutionFound)

      x.value should equalWithTolerance(Some(150))
      y.value should equalWithTolerance(Some(80))

      solver.objectiveValue should equal(Success(-2*150 + 5*80))
      solver.solutionQuality should equal(Success(Optimal))

      solver.release()
    }
  }

  test("Add multiple constraints") {
    for (_solver <- MPSolver.lpSolvers) {
      implicit val solver: MPSolver[_] = _solver

      val x = FloatVar("x", 0, 100)
      val y = FloatVar("y", 0, 100)
      val z = FloatVar("z", 0, 100)

      maximize(1 * x + 2 * y + 3 * z)
      add(x + y <= 75)
      add(x + z <= 75)

      val endStatus = solver.solve

      endStatus should equal(SolutionFound)

      x.value should equalWithTolerance(Some(0))
      y.value should equalWithTolerance(Some(75))
      z.value should equalWithTolerance(Some(75))

      solver.objectiveValue should equal(Success(1*0 + 2*75 + 3*75))
      solver.solutionQuality should equal(Success(Optimal))

      solver.release()
    }
  }

  test("Detect infeasible problem") {
    for (_solver <- MPSolver.lpSolvers) {
      implicit val solver: MPSolver[_] = _solver

      val x = FloatVar("x", 0, 10)
      val y = FloatVar("y", 80, 170)

      minimize(-2 * x + 5 * y)
      add(x + y >= 200)

      val endStatus = solver.solve

      endStatus should equal(Infeasible)

      intercept[NoSolutionFound] {
        solver.solutionQuality
      }

      solver.release()
    }
  }

  test("Detect unbounded problem") {
    for (_solver <- MPSolver.lpSolvers) {
      implicit val solver: MPSolver[_] = _solver

      val x = FloatVar("x")
      val y = FloatVar("y", 80, 170)

      minimize(-2 * x + 5 * y)
      add(x + y >= 200)

      val endStatus = solver.solve

      endStatus should equal(Unbounded)

      intercept[NoSolutionFound] {
        solver.solutionQuality
      }

      solver.release()
    }
  }

  test("Update variable bounds") {
    for (_solver <- MPSolver.lpSolvers) {
      implicit val solver: MPSolver[_] = _solver

      val x = FloatVar("x", 100, 150)
      val y = FloatVar("y", 80, 170)

      maximize(-2 * x + 5 * y)
      add(x + y <= 200)

      val endStatus = solver.solve

      endStatus should equal(SolutionFound)

      x.value should equalWithTolerance(Some(100))
      y.value should equalWithTolerance(Some(100))

      solver.objectiveValue should equal(Success(-2*100 + 5*100))
      solver.solutionQuality should equal(Success(Optimal))

      // Update bounds
      x.lowerBound = 0
      y.upperBound = 250

      // Model has changed
      solver.solved should equal(false)
      solver.hasSolution should equal(false)

      // Reoptimize to get new solution
      val endStatus2 = solver.solve

      endStatus should equal(SolutionFound)

      x.value should equalWithTolerance(Some(0))
      y.value should equalWithTolerance(Some(200))

      solver.objectiveValue should equal(Success(-2*0 + 5*200))
      solver.solutionQuality should equal(Success(Optimal))

      solver.release()
    }
  }

  test("Update objective") {
    for (_solver <- MPSolver.lpSolvers) {
      implicit val solver: MPSolver[_] = _solver

      val x = FloatVar("x", 100, 150)
      val y = FloatVar("y", 80, 170)

      maximize(-2 * x + 5 * y)
      add(x + y <= 200)

      val endStatus = solver.solve

      endStatus should equal(SolutionFound)

      x.value should equalWithTolerance(Some(100))
      y.value should equalWithTolerance(Some(100))

      solver.objectiveValue should equal(Success(-2*100 + 5*100))
      solver.solutionQuality should equal(Success(Optimal))

      // Update objective
      solver.objective = 2 * x - 5 * y

      // Model has changed
      solver.solved should equal(false)
      solver.hasSolution should equal(false)

      // Reoptimize to get new solution
      val endStatus2 = solver.solve

      endStatus should equal(SolutionFound)

      x.value should equalWithTolerance(Some(120))
      y.value should equalWithTolerance(Some(80))

      solver.objectiveValue should equal(Success(2*120 - 5*80))
      solver.solutionQuality should equal(Success(Optimal))

      solver.release()
    }
  }
}
