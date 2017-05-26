package oscar.xcsp3.test

import oscar.cp._
import oscar.xcsp3._
import oscar.xcsp3.testUtils.TestSuite

import scala.collection.mutable.ArrayBuffer


class TestXCSP3Parser extends TestSuite {




  def testSolution(instancePath: String, solution: String): Boolean = {
    new CheckerLib(instancePath, solution).valid
  }

  def isValid(instancePath: String, nSol: Int = 1): Boolean = {
    val parser = XCSP3Parser(instancePath)

    val vars = parser.varHashMap.values.toArray
    parser.cp.search {
      conflictOrderingSearch(vars,i => vars(i).size, i => vars(i).min)
    }

    var solutions = ArrayBuffer[String]()
    parser.cp.onSolution {
      val str1 = "<instantiation>\n\t<list>\n\t\t"
      val elems = vars.map(x => x.name).mkString(" ")
      var str2 = "\n\t</list>\n\t<values>\n\t\t"
      val vals = vars.map(x => x.value.toString).mkString(" ")
      val str3 = "\n\t</values>\n</instantiation>"
      solutions += str1 + elems + str2 + vals + str3
    }

    parser.cp.start(nSol)

    assert(solutions.size > 0) // feasible problem
    solutions.forall(sol => testSolution(instancePath, sol))
  }


  val KOTests = Array("Allergy.xml", // contains Intentional cst
    "Bibd-sum-06-050-25-03-10.xml", // take long time
    "ChessboardColoration-07-07.xml", // contains NotAllEqual cst
    "MagicSquare-6-mdd.xml", // take long time
    "MagicSquare-9-f10-01.xml", // take long time
    "PrizeCollecting-15-3-5-0.xml", // contains Short Table cst
    "RadarSurveillance-8-24-3-2-00.xml", // contains Intentional cst
    "StripPacking-C1P1.xml", // contains NoOverlap2D cst
    "Tpp-3-3-20-1.xml", // contains Circuit cst
    "Vrp-A-n32-k5.xml", // take long time
    "GolombRuler-09-a4.xml", // contains Intentional cst
    "Mario-easy-4.xml", // contains Circuit cst
    "Sat-flat200-00-clause.xml", // contains Clause cst
    "Ramsey-12.xml" // contains NotAllEqual cst
    )


  // objectif: ramener tout dans OKTests ;-)
  val OKTests = Array(
    "Pb-robin08.xml",
    "RoomMate-sr0050-int.xml",
    "StillLife-03-06.xml",
    "StillLife-wastage-03.xml",
    "Nonogram-001-table.xml",
    "Ortholatin-005.xml",
    "Zebra.xml",
    "LangfordBin-08.xml",
    "BusScheduling-cnt-t1.xml",
    "CarSequencing-dingbas.xml",
    "Pb-gr-05.xml",
    "Primes-15-20-2-1.xml",
    "Queens-0008-m1.xml",
    "testExtension1.xml",
    "testExtension2.xml",
    "BinPacking-mdd-ft060-00.xml",
    "BinPacking-mdd-n1c1w4a.xml",
    "Sudoku-s01a-alldiff.xml",
    "AllInterval-005.xml",
    "GolombRuler-09-a3.xml",
    "Hanoi-05.xml",
    "Cutstock-small.xml",
    "QueenAttacking-06.xml",
    "Taillard-os-04-04-0.xml",
    "Langford-3-10.xml",
    "Warehouse-opl.xml",
    "testPrimitive.xml",
    "QuasiGroup-7-09.xml",
    "QuasiGroup-3-04.xml",
    "MagicSequence-008-ca.xml",
    "MagicSequence-008-co.xml",
    "LowAutocorrelation-015.xml",
    "MagicSquare-4-table.xml",
    "Domino-300-300.xml",
    "driverlogw-09.xml",
    "Fastfood-ff10.xml",
    "ColouredQueens-07.xml",
    "CostasArray-12.xml",
    "Crossword-lex-vg-5-6.xml",
    "MarketSplit-01.xml",
    "Taillard-js-015-15-0.xml",
    "SportsScheduling-08.xml",
    "Bibd-sc-06-050-25-03-10.xml",
    "BinPacking-sum-n1c1w4a.xml",
    "BinPacking-tab-n1c1w4a.xml",
    "CryptoPuzzle-cross-roads-danger.xml",
    "DistinctVectors-30-050-02.xml",
    "GracefulGraph-K02-P04.xml",
    "GraphColoring-3-fullins-4.xml",
    "GraphColoring-qwhdec-o5-h10-1.xml",
    "Kakuro-easy-000-ext.xml",
    "Kakuro-easy-000-sumdiff.xml",
    "Knapsack-30-100-00.xml",
    "KnightTour-06-ext03.xml",
    "KnightTour-06-int.xml",
    "MultiKnapsack-1-0_X2.xml",
    "MultiKnapsack-1-01.xml",
    "qcp-15-120-00_X2.xml",
    "QuadraticAssignment-bur26a.xml",
    "QuadraticAssignment-qap.xml",
    "qwh-o30-h374-01.xml",
    "Subisomorphism-A-10.xml",
    "testObjective1.xml",
    "TravellingSalesman-20-30-00.xml",
    "Vrp-P-n16-k8.xml",
    "SocialGolfers-4-3-4-cp.xml",
    "Blackhole-04-3-00.xml",
    "Nonogram-001-regular.xml",
    "Opd-07-007-003.xml"
  )

  val OKTestsButSlow = Array(
    "Steiner3-08.xml"
  )




  for (t <- OKTests) {
    test(t) {
      assert(isValid("../data/xcsp3/instancesTest/"+t))
    }
  }

}
