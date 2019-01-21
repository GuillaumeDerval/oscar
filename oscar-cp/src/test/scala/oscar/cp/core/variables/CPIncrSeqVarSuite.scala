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
    val durations = Array.fill(3)(CPIntVar(1)(store))
    val transitions = Array.tabulate(3, 3)((i, j) => if(i == j) 0 else 3)
    val variable = new CPIncrSeqVar(store, starts, ends, durations, transitions)

    assert(variable.isPossible(0))
    assert(variable.isPossible(1))
    assert(variable.isPossible(2))
    val allPossible = variable.allPossible
    assertResult(3)(allPossible.length)
    assert(allPossible.contains(0))
    assert(allPossible.contains(1))
    assert(allPossible.contains(2))
  }

  test("No activity should be mandatory in the initial domain") {
    val store = new CPStore()
    val starts = Array.fill(3)(CPIntVar(0, 5)(store))
    val ends = Array.fill(3)(CPIntVar(1, 6)(store))
    val durations = Array.fill(3)(CPIntVar(1)(store))
    val transitions = Array.tabulate(3, 3)((i, j) => if(i == j) 0 else 3)
    val variable = new CPIncrSeqVar(store, starts, ends, durations, transitions)

    assert(!variable.isMandatory(0))
    assert(!variable.isMandatory(1))
    assert(!variable.isMandatory(2))
    assert(variable.allMandatory.isEmpty)
  }

  test("Only pickup activities should be visitable in the initial domain") {
    val store = new CPStore()
    val starts = Array.fill(4)(CPIntVar(0, 5)(store))
    val ends = Array.fill(4)(CPIntVar(1, 6)(store))
    val durations = Array.fill(4)(CPIntVar(1)(store))
    val transitions = Array.fill(4, 4)(2)
    val dependencies = Array(Seq(0, 2, 3))
    val variable = new CPIncrSeqVar(store, starts, ends, durations, transitions, dependencies)

    assert(variable.canVisit(0))
    assert(variable.canVisit(1))
    assert(!variable.canVisit(2))
    assert(!variable.canVisit(3))
    val allVisitable = variable.allVisitable
    assertResult(2)(allVisitable.length)
    assert(allVisitable.contains(0))
    assert(allVisitable.contains(1))
    assert(!allVisitable.contains(2))
    assert(!allVisitable.contains(3))
  }

  test("No activity should be possible in the initial domain if before minTime or after maxTime") {
    val store = new CPStore()
    val starts = Array(
      CPIntVar(0, 5)(store),
      CPIntVar(10, 15)(store),
      CPIntVar(20, 25)(store)
    )
    val ends = Array(
      CPIntVar(1, 6)(store),
      CPIntVar(11, 16)(store),
      CPIntVar(21, 26)(store)
    )
    val durations = Array.fill(3)(CPIntVar(1)(store))
    val transitions = Array.tabulate(3, 3)((i, j) => if(i == j) 0 else 3)
    val variable = new CPIncrSeqVar(store, starts, ends, durations, transitions, Array(), 10, 20)

    assert(!variable.isPossible(0))
    assert(variable.isPossible(1))
    assert(!variable.isPossible(2))
    val allPossible = variable.allPossible
    assertResult(1)(allPossible.length)
    assert(!allPossible.contains(0))
    assert(allPossible.contains(1))
    assert(!allPossible.contains(2))
  }

  test("No activity should be visited in the initial domain") {
    val store = new CPStore()
    val starts = Array.fill(3)(CPIntVar(0, 5)(store))
    val ends = Array.fill(3)(CPIntVar(1, 6)(store))
    val durations = Array.fill(3)(CPIntVar(1)(store))
    val transitions = Array.tabulate(3, 3)((i, j) => if(i == j) 0 else 3)
    val variable = new CPIncrSeqVar(store, starts, ends, durations, transitions)

    assert(variable.visited.isEmpty)
  }

  test("Visiting activities") {
    val store = new CPStore()
    val starts = Array.fill(3)(CPIntVar(0, 5)(store))
    val ends = Array.fill(3)(CPIntVar(1, 6)(store))
    val durations = Array.fill(3)(CPIntVar(1)(store))
    val transitions = Array.tabulate(3, 3)((i, j) => if(i == j) 0 else 3)
    val variable = new CPIncrSeqVar(store, starts, ends, durations, transitions)

    variable.visit(0)
    assert(variable.isVisited(0))
    assert(!variable.isPossible(0))
    assert(!variable.canVisit(0))
    var visited = variable.visited
    assertResult(1)(visited.length)
    assertResult(0)(visited.head)
    assertResult(1)(variable.currentEndTime)

    variable.visit(1)
    assert(variable.isVisited(1))
    assert(!variable.isPossible(1))
    assert(!variable.canVisit(1))
    visited = variable.visited
    assertResult(2)(visited.length)
    assertResult(0)(visited.head)
    assertResult(1)(visited.last)
    assertResult(5)(variable.currentEndTime)
  }

  
}