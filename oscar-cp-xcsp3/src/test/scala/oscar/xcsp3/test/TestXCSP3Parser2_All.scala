package oscar.xcsp3.test

import java.io.File

import oscar.modeling.models.UninstantiatedModel
import oscar.modeling.solvers.cp.{Branchings, CPProgram}
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.xcsp3._
import oscar.xcsp3.testUtils.TestSuite

class TestXCSP3Parser2_All extends TestSuite {

  def testSolution(instancePath: String, solution: String): Boolean = {
    println(solution)
    new CheckerLib(instancePath, solution).valid
  }

  def isValid(instancePath: String, nSol: Int = 1, useStaticOrdering: Boolean = false): Boolean = {
    val cpProgram = new CPProgram[String]()
    val (vars, solutionGenerator) =  XCSP3Parser2.parse(cpProgram.modelDeclaration, instancePath)

    val v = vars.toArray
    if(useStaticOrdering)
      cpProgram.setSearch(Branchings.binaryStatic(v))
    else
      cpProgram.setSearch(Branchings.conflictOrderingSearch(v, i => v(i).size, i => v(i).min))
    cpProgram.onSolution {
      solutionGenerator()
    }

    cpProgram.onSolution {
      val r = solutionGenerator()
      //println(r)
      r
    }

    cpProgram.setDecompositionStrategy(new CartProdRefinement(vars, Branchings.binaryFirstFail(vars.toSeq)))

    val (stats, solutions) = cpProgram.solveParallel(cpProgram.modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel], threadCount = 1, sppw = 1, nSols = 1, maxTime = 30000)

    assert(solutions.nonEmpty) // feasible problem
    solutions.forall(sol => testSolution(instancePath, sol))
  }

  def getFolderContent(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.toList.sortBy(_.getName)
    } else {
      List[File]()
    }
  }

  val active = false
  val startFirstN = 3


  // Currently ignored:
  // - Bibd/Bibd-sum-open (slow)
  // - Bibd/Bibd-open (slow)
  // - BinPacking/Binpacking-mdd-fu (MDD)
  // - BinPacking/Binpacking-mdd-sw100 (MDD)
  // - BinPacking/BinPacking-mdd-sw120 (MDD)
  // - BinPacking/BinPacking-sum-*** (slow)
  // - BinPacking/BinPacking-tab-*** (slow)


  if(active) {
    // Select startFirstN instances from all the families
    val inputs = getFolderContent("/Users/dervalguillaume/Downloads/xcsp3Download-Oct-17-2016-19-21-37")
      .filter(_.isDirectory) //get directories
      .map(f => (f.getName, f)) //store their name somewhere
      .flatMap(x => getFolderContent(x._2.getAbsolutePath).filter(_.isDirectory).map(f => (x._1+"/"+f.getName, f)))
      .flatMap(x => getFolderContent(x._2.getAbsolutePath).filter(_.getName.endsWith(".lzma")).take(startFirstN).map(f => (x._1+"/"+f.getName, f.getAbsolutePath)))

    for ((name, path) <- inputs) {
      test(name) {
        assert(isValid(path))
      }
    }
  }
}
