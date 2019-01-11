package oscar.cp.core.variables

import oscar.cp.core.{CPPropagStrength, CPStore, Constraint}
import oscar.cp.isInconsistent
import oscar.cp.testUtils._

import scala.util.Random

class CPIncrSeqVarSuite extends TestSuite {

  test("All activities should be possible in the initial domain") {
    val store = new CPStore()
    val starts = Array.fill(3)(CPIntVar(0, 5)(store))
    val ends = Array.fill(3)(CPIntVar(1, 6)(store))
    val transitions = Array.fill(3, 3)(0)
    val variable = new CPIncrSeqVar(store, starts, ends, transitions)
    assert(variable.isPossible(0))
    assert(variable.isPossible(1))
    assert(variable.isPossible(2))
  }

  test("No activity should be mandatory in the initial domain") {
    val store = new CPStore()
    val starts = Array.fill(3)(CPIntVar(0, 5)(store))
    val ends = Array.fill(3)(CPIntVar(1, 6)(store))
    val transitions = Array.fill(3, 3)(0)
    val variable = new CPIncrSeqVar(store, starts, ends, transitions)
    assert(!variable.isMandatory(0))
    assert(!variable.isMandatory(1))
    assert(!variable.isMandatory(2))
  }

  test("Only pickup activities should be present in next in the initial domain") {
    val store = new CPStore()
    val starts = Array.fill(4)(CPIntVar(0, 5)(store))
    val ends = Array.fill(4)(CPIntVar(1, 6)(store))
    val transitions = Array.fill(4, 4)(0)
    val dependencies = Array((0, 2), (1, 4))
    val variable = new CPIncrSeqVar(store, starts, ends, transitions, dependencies)
    assert(variable.isPossibleNext(0))
    assert(variable.isPossibleNext(1))
    assert(!variable.isPossibleNext(2))
    assert(!variable.isPossibleNext(4))
  }


  
}