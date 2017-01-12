package oscar.cbls.test.invariants

import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.core.computation.{CBLSSeqVar, Store}
import oscar.cbls.core.propagation.ErrorChecker
import oscar.cbls.lib.invariant.seq._

/**
 * Created by rdl on 27-07-16.
 */
object testConcat extends App {

  val m = new Store(verbose = true,propagateOnToString = true, checker = Some(new ErrorChecker()))
  val a = new CBLSSeqVar(m,IntSequence(List(1,2,3,5)), n = "toto")

  val concat = new ConcatenateFirstConstant(List(1,6,8,30),a,4,20)

  m.close()

  println(m.stats)

  println(concat)

  println("insertAtPosition(45,3)")

  a.insertAtPosition(45,3)

  println(concat)

}
