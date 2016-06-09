package oscar.cp.constraints

import oscar.cp.testUtils.TestSuite
import oscar.cp._
import oscar.cp.core.CPOutcome

class SubCircuitSuite extends TestSuite {
  
  private def testData(nSuccs: Int): (CPSolver, Array[CPIntVar]) = {
    val cp = CPSolver()
    val succs = Array.fill(nSuccs)(CPIntVar(0 until nSuccs)(cp))
    cp.post(SubCircuit(succs))
    (cp, succs)
  }
  
  test("one") {
    val (cp, succs) = testData(1)
    assert(succs.head.value == 0)
    assert(!cp.isFailed)
  }
  
  test("no circuit") {
    val (cp, succs) = testData(10)
    for (i <- 0 until 10) {
      cp.add(succs(i) == i)
    }
    assert(!cp.isFailed)
  }
  
  test("circuit") {
    val (cp, succs) = testData(5)
    cp.add(succs(0) == 1)
    cp.add(succs(1) == 2)
    cp.add(succs(2) == 3)
    cp.add(succs(3) == 4)
    cp.add(succs(4) == 0)
    assert(!cp.isFailed)
  } 
  
  test("subcircuit 1") {
    val (cp, succs) = testData(5)
    cp.add(succs(0) == 1)
    cp.add(succs(1) == 2)
    cp.add(succs(2) == 0)
    assert(!cp.isFailed)
    assert(succs(3).value == 3)
    assert(succs(4).value == 4)
  }
  
  test("subcircuit 2") {
    val (cp, succs) = testData(5)
    cp.add(succs(3) == 3)
    cp.add(succs(0) == 1)
    cp.add(succs(1) == 2)
    cp.add(succs(2) == 0)
    assert(!cp.isFailed)
    assert(succs(4).value == 4)
  }
  
  test("only one subtour") {
    val (cp, succs) = testData(5)
    cp.add(succs(0) == 1)
    cp.add(succs(1) == 2)
    cp.add(succs(3) == 4)
    assert(!cp.isFailed)
    assert(succs(4).value == 0)
    assert(succs(2).value == 3)
  }

  test("issue detected by Damien Mercier") {
    implicit val cp = CPSolver()
    /*
      Two subcircuits :
      - (2->3->4->2)
      - (1->5->1)
      - And elem 0 is not taken (0 -> 0)
     */
    val successors = Array(
      CPIntVar(0),//0
      CPIntVar(5),//1
      CPIntVar(3),//2
      CPIntVar(4),//3
      CPIntVar(2),//4
      CPIntVar(1) //5
    )

    assert(cp.post(subCircuit(successors)) == CPOutcome.Failure)



    def checkUniqueCircuit(): Boolean = {
      val takenSuccessorsIndices = successors.indices.filter(i => !successors(i).isBoundTo(i))
      val n=takenSuccessorsIndices.length
      if(n==0){return true}//Empty circuit
      val first = takenSuccessorsIndices.head
      var curr = first
      var c = 0
      do{
        curr = successors(curr).value
        c+=1
      } while(curr != first)
      if(c!=n){
        println(takenSuccessorsIndices.map(i=>(i,successors(i).value)).mkString(","))
        println(s"Number of successors in one subcircuit:$c")
        println(s"Total number of successors in all subcircuits:$n")
        System.out.flush()
        return false
      }
      return true
    }

    //assert(checkUniqueCircuit())

  }
  
  test("solve all") {
    val (cp, succs) = testData(6)
    cp.search(binaryFirstFail(succs))
    val stats = cp.start()
    assert(stats.nSols == nSubCircuits(6))
  }
  
  private def nSubCircuits(n: Int): Int = {
    1 + (2 to n).map(i => combinations(n, i) * factorial(i-1)).sum
  }
  
  private def combinations(n: Int, k: Int): Int = {
    factorial(n)/(factorial(n-k)*factorial(k))
  }
  
  private def factorial(n: Int): Int = {
    if (n == 0) 1 
    else factorial(n, 1)
  }
  
  @annotation.tailrec
  private def factorial(n: Int, cum: Int): Int = {
    if (n == 1) cum
    else factorial(n-1, cum*n)
  }
}