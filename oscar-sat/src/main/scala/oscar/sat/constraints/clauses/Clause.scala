package oscar.sat.constraints.clauses

import oscar.algo.array.ArrayStackInt
import oscar.sat.core.CDCLStore
import oscar.sat.constraints.Constraint

abstract class Clause extends Constraint {
  
  def nLiterals: Int
  
}

object Clause {
  
  def apply(solver: CDCLStore, literals: Array[Int]): Clause = {
    if (literals.length == 2) binary(solver, literals(0), literals(1))
    else watchLiterals(solver, literals)
  }

  def apply(solver: CDCLStore, literals: ArrayStackInt): Clause = {
    if (literals.length == 2) binary(solver, literals(0), literals(1))
    else watchLiterals(solver, literals.toArray)
  }
  
  def watchLiterals(solver: CDCLStore, literals: Array[Int]): Clause = {
    new WLClause(solver, literals)
  }
  
  def binary(solver: CDCLStore, lit1: Int, lit2: Int): Clause = {
    new BinaryClause(solver, lit1, lit2)
  }
}