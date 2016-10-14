package oscar.xcsp3.test

import oscar.modeling.solvers.cp.DistributedCPProgram
import oscar.modeling.solvers.cp.branchings.Branching
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.xcsp3._
import oscar.xcsp3.testUtils.TestSuite

class TestXCSP3Parser2 extends TestSuite {

  def testSolution(instancePath: String, solution: String): Boolean = {
    new CheckerLib(instancePath, solution).valid
  }

  def isValid(instancePath: String, nSol: Int = 1): Boolean = {
    val cpProgram = new DistributedCPProgram[String]()
    val (vars, solutionGenerator) =  XCSP3Parser2.parse(cpProgram.modelDeclaration, instancePath)

    cpProgram.setSearch(Branching.binaryFirstFail(vars.toSeq))
    cpProgram.onSolution {
      solutionGenerator()
    }

    cpProgram.onSolution {
      val r = solutionGenerator()
      println(r)
      r
    }

    cpProgram.setDecompositionStrategy(new CartProdRefinement(vars, Branching.binaryFirstFail(vars.toSeq)))

    val (stats, solutions) = cpProgram.solveLocally(cpProgram.modelDeclaration.getCurrentModel, 1, 1)

    assert(solutions.nonEmpty) // feasible problem
    solutions.forall(sol => testSolution(instancePath, sol))
  }


  val KOTests = Array(
    //Too slow (need OscaR-Modeling support for stop conditions)
    //"ColouredQueens-07.xml",
    //"CostasArray-12.xml",
    //"driverlogw-09.xml",
    //"GracefulGraph-K02-P04.xml",
    //"GraphColoring-3-fullins-4.xml",
    //"Hanoi-05.xml",
    //"MagicSquare-6-sum.xml",
    //"MagicSquare-9-f10-01.xml",
    //"MarketSplit-01.xml",
    //"qcp-15-120-00_X2.xml",
    //"QuadraticAssignment-bur26a.xml",
    //"QuasiGroup-7-09.xml",
    //"qwh-o30-h374-01.xml",
    //"RadarSurveillance-8-24-3-2-00.xml",
    //"Steiner3-08.xml",
    //"TravellingSalesman-20-30-00.xml",
    //"BinPacking-sum-n1c1w4a.xml",
    //"BinPacking-tab-n1c1w4a.xml",
    //"KnightTour-06-int.xml",
    //"QueenAttacking-06.xml",
    //"Fastfood-ff10.xml",
    //"Vrp-A-n32-k5.xml",
    //"Vrp-P-n16-k8.xml",
    //"Pb-robin08.xml", //file not found

    // Not working yet, lacking constraint implementation
    "BinPacking-mdd-n1c1w4a.xml",
    "Blackhole-04-3-00.xml",
    "ChessboardColoration-07-07.xml",
    "Crossword-lex-vg-5-6.xml",
    "DistinctVectors-30-050-02.xml",
    "Domino-300-300.xml",
    "MagicSquare-6-mdd.xml",
    "Nonogram-001-regular.xml",
    "Ramsey-12.xml",
    "Sat-flat200-00-clause.xml"

    // Too long, does not find solutions
    //"Taillard-js-015-15-0.xml",
    //"StripPacking-C1P1.xml"
  )


  // objectif: ramener tout dans OKTests ;-)
  val OKTests = Array("testExtension1.xml",
    "testExtension2.xml",
    "Allergy.xml",
    "AllInterval-005.xml",
    "CarSequencing-dingbas.xml",
    "Cutstock-small.xml",
    "GolombRuler-09-a3.xml",
    "GolombRuler-09-a4.xml",
    "GraphColoring-qwhdec-o5-h10-1.xml",
    "Kakuro-easy-000-ext.xml",
    "Kakuro-easy-000-sumdiff.xml",
    "Knapsack-30-100-00.xml",
    "Langford-3-10.xml",
    "LangfordBin-08.xml",
    "LowAutocorrelation-015.xml",
    "MultiKnapsack-1-0_X2.xml",
    "MultiKnapsack-1-01.xml",
    "Nonogram-001-table.xml",
    "Ortholatin-005.xml",
    "Pb-gr-05.xml",
    "Primes-15-20-2-1.xml",
    "QuadraticAssignment-qap.xml",
    "QuasiGroup-3-04.xml",
    "Queens-0008-m1.xml",
    "StillLife-03-06.xml",
    "StillLife-wastage-03.xml",
    "Subisomorphism-A-10.xml",
    "Sudoku-s01a-alldiff.xml",
    "testObjective1.xml",
    "testPrimitive.xml",
    "Zebra.xml",
    "MagicSequence-008-ca.xml",
    "CryptoPuzzle-cross-roads-danger.xml",
    "BusScheduling-cnt-t1.xml",
    "MagicSequence-008-co.xml",
    "Warehouse-opl.xml",
    "RoomMate-sr0050-int.xml",
    "PrizeCollecting-15-3-5-0.xml",
    "Mario-easy-4.xml",
    "Tpp-3-3-20-1.xml",
    "Opd-07-007-003.xml",
    "Taillard-os-04-04-0.xml"

    //"Bibd-sc-06-050-25-03-10.xml", //too many solutions, but works
    //"Bibd-sum-06-050-25-03-10.xml", //too many solutions, but works
    //"KnightTour-06-ext03.xml", //too many solutions, but works
    //"MagicSquare-4-table.xml", //too many solutions, but works
    //"SportsScheduling-08.xml", //too many solutions, but works
    //"SocialGolfers-4-3-4-cp.xml", //too many solutions, but works


    //"testExtension3.xml", //has no solutions
  )


  for (t <- KOTests) {
    test(t) {
      assert(isValid("data/xcsp3/instancesTest/"+t))
    }
  }

}
