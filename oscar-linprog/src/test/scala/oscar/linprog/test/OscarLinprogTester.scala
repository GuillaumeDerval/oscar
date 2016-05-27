package oscar.linprog.test

import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}
import oscar.linprog.interface.{MPSolverInterface, MPSolverLib}
import oscar.linprog.modeling.MPSolver

/**
 * Helper class for tests.
 */
class OscarLinprogTester extends FunSuite with Matchers with OscarLinprogMatchers with BeforeAndAfter {

  def testForAllSolvers[I <: MPSolverInterface](solverLibs: Seq[MPSolverLib[I]], desc: String)(body: MPSolver[I] => Unit) =
    for {
      solverLib <- solverLibs
    } {
      test(s"[${solverLib.name}] - " + desc){
        val solver = new MPSolver(solverLib.createSolver)
        body(solver)
        solver.release()
      }
    }

}
