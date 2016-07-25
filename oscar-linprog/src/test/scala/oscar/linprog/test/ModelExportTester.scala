package oscar.linprog.test

import java.nio.file.Paths

import oscar.algebra._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import oscar.linprog.enums.{LP, MPS, ModelExportFormat}
import oscar.linprog.interface.MPSolverLib
import oscar.linprog.interface.gurobi.GurobiLib
import oscar.linprog.modeling._

@RunWith(classOf[JUnitRunner])
class ModelExportTester extends OscarLinprogTester {

  def exportPath(format: ModelExportFormat) = Paths.get(s"test.${format.extension}")

  after {
    for (format <- ModelExportFormat.formats) {
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

      maximize(-2.0(x) + 5(y))
      add(s"C_${solver.getNumberOfLinearConstraints}" ||:x + y <= 200)

      solver.exportModel(exportPath(format), format)
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

      maximize(-2(x) + 5(y))
      add(s"C_${solver.getNumberOfLinearConstraints}" ||:x + y <= 200)

      solver.solve

      solver.exportModel(exportPath(format), format)
    }
  }

  if(MPSolverLib.canInstantiate(GurobiLib)) {
    test("Export model should fail for Gurobi if the file does not have the correct extension") {
      intercept[IllegalArgumentException] {

        implicit val solver = new MPSolver(GurobiLib.createSolver)

        val x = MPFloatVar("x", 100, 150)
        val y = MPFloatVar("y", 80, 170)

        maximize(-2(x) + 5(y))
        add(s"C_${solver.getNumberOfLinearConstraints}" ||:x + y <= 200)

        solver.exportModel(exportPath(MPS), LP)
      }
    }
  }
}
