package oscar.cp.core.variables

import oscar.cp.testUtils._
import oscar.algo.search.Outcome._
import scala.util.Random
import oscar.cp.core.CPStore

class CPBoolVarSuite extends TestSuite {
  
  test("updateMin should update the domain if necessary") {
    val store = new CPStore()
    val b1 = CPBoolVar()(store)
    val b2 = CPBoolVar(true)(store)
    assert(b1.updateMin(1) == Suspend)
    assert(b1.min == 1)
    assert(b2.updateMin(1) == Suspend)
  }
  
  test("updateMax should update the domain if necessary") {
    val store = new CPStore()
    val b1 = CPBoolVar()(store)
    val b2 = CPBoolVar(false)(store)
    assert(b1.updateMax(0) == Suspend)
    assert(b1.max == 0)
    assert(b2.updateMax(0) == Suspend)
  }
  
  test("updateMin should fail if the value is greater than 1") {
    val store = new CPStore()
    val b = CPBoolVar()(store)
    assert(b.updateMin(2) == Failure)
  }
  
  test("updateMax should fail if the value is lower than 0") {
    val store = new CPStore()
    val b = CPBoolVar()(store)
    assert(b.updateMax(-1) == Failure)
  }
  
  test("updateMin should fail if the value is greater than the assigned value") {
    val store = new CPStore()
    val b = CPBoolVar(false)(store)
    assert(b.updateMin(1) == Failure)
  }
  
  test("updateMax should fail if the value is lower than the assigned value") {
    val store = new CPStore()
    val b = CPBoolVar(true)(store)
    assert(b.updateMax(0) == Failure)
  }
}