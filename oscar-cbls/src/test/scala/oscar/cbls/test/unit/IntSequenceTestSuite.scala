package oscar.cbls.test.unit

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import oscar.cbls.algo.seq.IntSequence

class IntSequenceTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  test("t"){
    val seq = IntSequence(List(1,2,3,4))
    seq.contains(2) should be (true)
  }

}
