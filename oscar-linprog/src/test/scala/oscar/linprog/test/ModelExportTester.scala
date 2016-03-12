package oscar.linprog.test

import java.nio.file.Paths

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import oscar.linprog.interface.MPSolverLib
import oscar.linprog.modeling._

@RunWith(classOf[JUnitRunner])
class ModelExportTester extends OscarLinprogTester {

  def exportPath(format: String) = Paths.get(s"test.$format")

  after {
    for (format <- Seq("lp", "mps")) {
      exportPath(format).toFile.delete()
    }
  }

  test("Export model before solve") {
    for {
      solverLib <- MPSolverLib.solvers
      format <- solverLib.supportedModelExportFormats
    } {
      implicit val solver = new MPSolver(solverLib.createSolver)

      val x = MPFloatVar("x", 100, 150)
      val y = MPFloatVar("y", 80, 170)

      maximize(-2 * x + 5 * y)
      add(x + y <:= 200)

      solver.exportModel(exportPath(format))
    }
  }

  test("Export model after solve") {
    for {
      solverLib <- MPSolverLib.solvers
      format <- solverLib.supportedModelExportFormats
    } {
      implicit val solver = new MPSolver(solverLib.createSolver)

      val x = MPFloatVar("x", 100, 150)
      val y = MPFloatVar("y", 80, 170)

      maximize(-2 * x + 5 * y)
      add(x + y <:= 200)

      solver.solve

      solver.exportModel(exportPath(format))
    }
  }

  test("Export model should fail if the file does not have the correct extension") {
    for {
      solverLib <- MPSolverLib.solvers
      format <- solverLib.supportedModelExportFormats
    } {
      intercept[IllegalArgumentException] {
        implicit val solver = new MPSolver(solverLib.createSolver)

        val x = MPFloatVar("x", 100, 150)
        val y = MPFloatVar("y", 80, 170)

        maximize(-2 * x + 5 * y)
        add(x + y <:= 200)

        solver.exportModel(exportPath("wrongFormat"))
      }
    }
  }
}
